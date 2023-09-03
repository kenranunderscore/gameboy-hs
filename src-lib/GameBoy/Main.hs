{-# LANGUAGE OverloadedStrings #-}

module GameBoy.Main (main) where

import Control.Concurrent.Async qualified as Async
import Control.Exception qualified as Exception
import Control.Monad
import Control.Monad.State.Strict
import Optics
import SDL (($=))
import SDL qualified
import System.Environment qualified as Environment

import GameBoy.BitStuff
import GameBoy.CPU
import GameBoy.Memory
import GameBoy.PPU

maxCyclesPerFrame :: Int
maxCyclesPerFrame = 69_905

mainLoop :: (MonadIO m, CPU m) => m ()
mainLoop = forever $ do
    oneFrame 0
    renderScreen
  where
    oneFrame n = when (n < maxCyclesPerFrame) $ do
        s <- get
        instr <- fetch
        cycles <- execute instr -- TODO: this should return cycles it took
        liftIO $ putStrLn $ toHex (view programCounter s) <> " :  " <> show instr
        void $ updateTimers cycles
        void $ updateGraphics cycles
        void $ handleInterrupts
        oneFrame (n + cycles)
    renderScreen = pure () -- TODO

main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        [] -> fail "need path to ROM as first argument"
        (cartridgePath : _) -> do
            graphics <- Async.async $ runGraphics
            game <- Async.async $ do
                bus <- initializeMemoryBus cartridgePath
                _finalRegisters <- execStateT mainLoop (mkInitialState bus)
                pure ()
            void $ Async.waitAnyCancel [graphics, game]

runGraphics :: IO ()
runGraphics = do
    withSdl $ withSdlWindow $ \w ->
        withSdlRenderer w $ \renderer -> do
            SDL.rendererDrawColor renderer $= SDL.V4 0 0 255 255
            void $ appLoop renderer
  where
    escPressed evt =
        case SDL.eventPayload evt of
            SDL.KeyboardEvent keyboardEvent ->
                SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
                    && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeEscape
            _ -> False
    appLoop renderer = do
        evts <- SDL.pollEvents
        SDL.clear renderer
        SDL.present renderer
        unless (any escPressed evts) (appLoop renderer)

withSdl :: IO a -> IO a
withSdl =
    Exception.bracket_
        ( do
            SDL.initialize [SDL.InitVideo]
            putStrLn "SDL initialized"
        )
        ( do
            SDL.quit
            putStrLn "Shut down SDL"
        )

withSdlWindow :: (SDL.Window -> IO a) -> IO a
withSdlWindow =
    Exception.bracket
        (SDL.createWindow "GameBoy emulator" SDL.defaultWindow)
        ( \w -> do
            SDL.destroyWindow w
            putStrLn "Window destroyed"
        )

withSdlRenderer :: SDL.Window -> (SDL.Renderer -> IO a) -> IO a
withSdlRenderer window =
    Exception.bracket
        (SDL.createRenderer window (-1) SDL.defaultRenderer)
        ( \r -> do
            SDL.destroyRenderer r
            putStrLn "Renderer destroyed"
        )
