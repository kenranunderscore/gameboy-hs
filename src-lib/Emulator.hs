{-# LANGUAGE LambdaCase #-}

module Emulator (run) where

import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import Data.Binary.Get

run :: IO ()
run = do
  let biosPath = "./resources/gb_bios.bin"
  bios <- LBS.readFile biosPath
  let rom = runGet getInstructions bios
  forM_ rom (putStrLn . printInstruction)

data Instruction
  = ADD | LD | UNKNOWN
  deriving (Eq, Show)

printInstruction :: Instruction -> String
printInstruction = \case
  ADD -> "ADD"
  LD -> "LD"
  UNKNOWN -> "UNKNOWN"

getInstruction :: Get Instruction
getInstruction = do
  b <- getWord8
  case b of
    49 -> pure LD
    _ -> pure UNKNOWN

getInstructions :: Get [Instruction]
getInstructions = do
  empty <- isEmpty
  if empty
    then pure []
    else do
      instr <- getInstruction
      rest <- getInstructions
      pure (instr : rest)
