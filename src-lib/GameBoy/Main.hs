{-# LANGUAGE OverloadedStrings #-}

module GameBoy.Main (main) where

import Control.Concurrent.Async qualified as Async
import Control.Monad
import Control.Monad.State.Strict
import Data.IORef
import Optics
import System.Environment qualified as Environment

import GameBoy.BitStuff
import GameBoy.CPU
import GameBoy.Memory
import GameBoy.PPU
import GameBoy.Render qualified as Render
import GameBoy.State

maxCyclesPerFrame :: Int
maxCyclesPerFrame = 69_905

mainLoop :: (MonadIO m, GameBoy m) => IORef InMemoryScreen -> m ()
mainLoop scrRef = forever $ do
    oneFrame 0
    liftIO $ putStrLn "    [FRAME FINISHED]"
    scr <- use screen
    liftIO $ writeIORef scrRef scr
  where
    oneFrame n = when (n < maxCyclesPerFrame) $ do
        s <- get
        instr <- fetch
        cycles <- execute instr
        liftIO $ putStrLn $ toHex (view programCounter s) <> " :  " <> show instr
        void $ updateTimers cycles
        void $ updateGraphics cycles
        void $ handleInterrupts
        oneFrame (n + cycles)

main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        [] -> fail "need path to ROM as first argument"
        (cartridgePath : _) -> do
            scrRef <- newIORef emptyScreen
            graphics <- Async.asyncBound $ Render.runGraphics scrRef
            game <- Async.async $ do
                bus <- initializeMemoryBus cartridgePath
                void $ execStateT (mainLoop scrRef) (mkInitialState bus)
            void $ Async.waitAnyCancel [graphics, game]
