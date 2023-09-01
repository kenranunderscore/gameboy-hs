module GameBoy.Main (main) where

import Control.Monad.State.Strict
import System.Environment qualified as Environment

import GameBoy.CPU
import GameBoy.Memory
import GameBoy.PPU

main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        [] -> fail "need path to ROM as first argument"
        (cartridgePath : _) -> do
            bus <- initializeMemoryBus cartridgePath
            finalRegisters <- execStateT startup (mkInitialState bus)
            putStrLn "done"
            print finalRegisters
