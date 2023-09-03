module GameBoy.Main (main) where

import Control.Monad
import Control.Monad.State.Strict
import Optics
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
            bus <- initializeMemoryBus cartridgePath
            _finalRegisters <- execStateT mainLoop (mkInitialState bus)
            pure ()
