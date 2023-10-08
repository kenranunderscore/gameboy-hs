{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module GameBoy.Main (main) where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.IORef
import Data.Time qualified as Time
import System.Environment qualified as Environment
import System.IO

import GameBoy.CPU
import GameBoy.Gamepad
import GameBoy.Memory
import GameBoy.PPU
import GameBoy.Render qualified as Render
import GameBoy.State

maxCyclesPerFrame :: Int
maxCyclesPerFrame = 4_194_304 `div` 60

mainLoop ::
    IORef InMemoryScreen ->
    STM.TVar GamepadState ->
    GameBoy ()
mainLoop scrRef buttonsRef = do
    now <- liftIO Time.getCurrentTime
    loop 0 now
  where
    loop (frames :: Int) t = do
        -- TODO: it feels wrong to start at 0 here
        -- shouldn't we use the superfluous cycles from the frame before?
        oneFrame 0
        scr <- gets (.preparedScreen)
        liftIO $ writeIORef scrRef scr
        if (frames == 300)
            then do
                now <- liftIO Time.getCurrentTime
                let
                    dt = Time.diffUTCTime now t
                    fps = realToFrac (frames + 1) / dt
                liftIO $ putStrLn $ "  FPS = " <> show fps
                loop 0 now
            else loop (frames + 1) t
    oneFrame n = when (n < maxCyclesPerFrame) $ do
        syncInput
        s <- get
        -- let
        --     serialControl = readByte s.memoryBus 0xff02
        --     msg' = if (serialControl == 0x81) then BS.cons (readByte s.memoryBus 0xff01) msg else msg
        -- when (serialControl == 0x81) $ do
        --     writeMemory 0xff02 0
        --     liftIO $ putStrLn $ "DBG: " <> reverse (Char8.unpack msg')
        cycles <-
            if not s.halted
                then do
                    instr <- fetch
                    cycles <- execute instr
                    -- liftIO $ putStrLn $ toHex (view programCounter s) <> " :  " <> show instr
                    -- liftIO $ putStrLn $ " DIV == " <> toHex (view (memoryBus % timers % divider) s `Bits.shiftR` 8)
                    pure cycles
                else do
                    -- liftIO $ putStrLn "  [HALT]"
                    when (interruptFlags s.memoryBus > 0) continue
                    pure 4
        updateTimers cycles
        updateGraphics cycles
        interruptCycles <- handleInterrupts
        oneFrame (n + cycles + interruptCycles)
    syncInput = do
        buttons <- liftIO $ STM.readTVarIO buttonsRef
        -- unless (Set.null buttons) (liftIO $ print buttons)
        modifyBusM $ \bus -> bus{gamepadState = buttons}

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    args <- Environment.getArgs
    case args of
        [] -> fail "need path to ROM as first argument"
        (cartridgePath : _) -> do
            scrRef <- newIORef emptyScreen
            buttonsRef <- STM.newTVarIO noButtonsPressed
            game <- Async.async $ do
                cart <- loadCartridgeFromFile cartridgePath
                let bus = mkMemoryBus cart.memory
                void $ runReaderT (execStateT (mainLoop scrRef buttonsRef) (mkInitialState bus)) cart
            graphics <-
                Async.asyncBound $
                    -- FIXME: try out something more performant than STM, as we
                    -- don't have concurrent writes
                    Render.runGraphics (STM.atomically . STM.writeTVar buttonsRef) scrRef
            void $ Async.waitAnyCancel [graphics, game]
