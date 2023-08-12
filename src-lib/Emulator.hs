{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Emulator (run) where

import Data.List
import qualified Data.ByteString.Lazy as LBS
import Data.Binary.Get

run :: IO ()
run = do
  let biosPath = "./resources/gb_bios.bin"
  bios <- LBS.readFile biosPath
  let rom = runGet getInstructions bios
  mapM_ print $
    takeWhile (\(Op s) -> not $ "UNKNOWN" `isInfixOf` s) rom

newtype Op = Op String
  deriving newtype Show

op = pure . Op

getCB :: Get Op
getCB = getWord8 >>= \case
  0x7c -> op "BIT 7,H"
  0x11 -> op "RL C"

getInstruction :: Get Op
getInstruction = getWord8 >>= \case
  0x04 -> op "INC B"
  0x05 -> op "DEC B"
  0x06 -> do
    _ <- getWord8
    op "LD B,u8"
  0x0c -> op "INC C"
  0x0d -> op "DEC C"
  0x0e -> do
    _ <- getWord8
    op "LD C,u8"
  0x11 -> do
    _ <- getWord16le
    op "LD DE,u16"
  0x13 -> op "INC DE"
  0x15 -> op "DEC D"
  0x16 -> do
    _ <- getWord8
    op "LD D,u8"
  0x17 -> op "RLA"
  0x1a -> op "LD A,(DE)"
  0x1e -> do
    _ <- getWord8
    op "LD E,u8"
  0x18 -> do
    _ <- getWord8 -- i8
    op "JR i8"
  0x1d -> op "DEC E"
  0x20 -> do
    _ <- getWord8
    op "JR NZ,i8"
  0x21 -> do
    _ <- getWord16le
    op "LD HL,u16"
  0x22 -> op "LD (HL+),A"
  0x23 -> op "INC HL"
  0x24 -> op "INC H"
  0x2e -> do
    _ <- getWord8
    op "LD L,u8"
  0x28 -> do
    _ <- getWord8 -- i8
    op "JR Z,i8"
  0x31 -> do
    _ <- getWord16le
    op "LD SP,u16"
  0x32 -> do op "LD (HL-),A"
  0x3d -> op "DEC A"
  0x3e -> do
    _ <- getWord8
    op "LD A,u8"
  0x4f -> op "LD C,A"
  0x56 -> op "LD D,(HL)"
  0x57 -> op "LD D,A"
  0x67 -> op "LD H,A"
  0x77 -> op "LD (HL),A"
  0x7b -> op "LD A,E"
  0x7c -> op "LD A,H"
  0x90 -> op "SUB A,B"
  0xaf -> op "XOR A"
  0xc1 -> op "POP BC"
  0xc5 -> op "PUSH BC"
  0xc9 -> op "RET"
  0xcb -> getCB
  0xcd -> do
    _ <- getWord16le
    op "CALL u16"
  0xe0 -> do
    _ <- getWord8
    op "LD (FF00+u8,A)"
  0xe2 ->
    op "LD (FF00+C,A)"
  0xea -> do
    _ <- getWord16le
    op "LD (u16),A"
  0xf0 -> do
    _ <- getWord8
    op "LD A,(FF00+u8)"
  0xfe -> do
    _ <- getWord8
    op "CP A,u8"
  unknown -> op $ "UNKNOWN: " <> show unknown

getInstructions :: Get [Op]
getInstructions = do
  empty <- isEmpty
  if empty
    then pure []
    else do
      instr <- getInstruction
      rest <- getInstructions
      pure (instr : rest)
