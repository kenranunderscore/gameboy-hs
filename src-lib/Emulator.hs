{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Emulator (run) where

import Control.Monad
import Data.Bits
import Data.Array ((!))
import Data.Word
import Control.Monad.State.Strict

import Memory

data Registers = Registers
  { a :: Byte
  , f :: Byte
  , b :: Byte
  , c :: Byte
  , d :: Byte
  , e :: Byte
  , h :: Byte
  , l :: Byte
  , pc :: Word16
  , sp :: Word16
  }
  deriving stock (Eq)

combineBytes :: Byte -> Byte -> Word16
combineBytes b1 b2 = (fromIntegral b1 .<<. 8) .|. fromIntegral b2

af :: Registers -> Word16
af r = combineBytes r.a r.f

bc :: Registers -> Word16
bc r = combineBytes r.b r.c

de :: Registers -> Word16
de r = combineBytes r.d r.e

hl :: Registers -> Word16
hl r = combineBytes r.h r.l

instance Show Registers where
  show r = mconcat
    [ "A  = ", toHex r.a, "\n"
    , "F  = ", toHex r.f
    , "    AF = ", toHex (af r), "\n"
    , "B  = ", toHex r.b, "\n"
    , "C  = ", toHex r.c
    , "    BC = ", toHex (bc r), "\n"
    , "D  = ", toHex r.d, "\n"
    , "E  = ", toHex r.e
    , "    DE = ", toHex (de r), "\n"
    , "H  = ", toHex r.h, "\n"
    , "L  = ", toHex r.l
    , "    HL = ", toHex (hl r), "\n"
    , "PC = ", toHex r.pc, "\n"
    , "SP = ", toHex r.sp, "\n"
    ]

initialRegisters :: Registers
initialRegisters = Registers 0 0 0 0 0 0 0 0 0 0

type CPU m = (MonadState Registers m, MonadFail m)

advance :: CPU m => Word16 -> m ()
advance n = do
  registers <- get
  put $ registers{pc = registers.pc + n}

data Instr
  = LD_SP_u16 Word16 -- TODO: replace flat instructions with a tree
  | LD_HL_u16 Word16
  | XOR_A

instance Show Instr where
  show = \case
    LD_SP_u16 u16 -> "LD SP," <> toHex u16
    LD_HL_u16 u16 -> "LD HL," <> toHex u16
    XOR_A -> "XOR A"

fetchU16 :: Memory -> Word16 -> Word16
fetchU16 mem addr = do
  let b1 = mem ! (addr + 1) -- little Endian
      b2 = mem ! addr
  (fromIntegral b1 .<<. 8) .|. fromIntegral b2

readInstr :: CPU m => Memory -> m Instr
readInstr mem = do
  counter <- gets pc
  case mem ! counter of
    0x21 -> do
      let u16 = fetchU16 mem (counter + 1)
      advance 3
      pure $ LD_HL_u16 u16
    0x31 -> do
      let u16 = fetchU16 mem (counter + 1)
      advance 3
      pure $ LD_SP_u16 u16
    0xaf -> advance 1 >> pure XOR_A
    b -> fail $ "unknown instruction byte: " <> showByte b

execute :: (MonadIO m, CPU m) => Instr -> m ()
execute instr = do
  registers <- get
  let newRegisters =
        case instr of
          XOR_A -> registers{a = registers.a `xor` registers.a}
          LD_SP_u16 u16 -> registers -- TODO: implement
          LD_HL_u16 u16 -> registers -- TODO: implement
  liftIO $ print registers
  put newRegisters

run :: IO ()
run = do
  finalRegisters <- execStateT startup initialRegisters
  putStrLn "done"
  print finalRegisters

startup :: (MonadIO m, CPU m) => m ()
startup = loop
  where
    loop = forever $ do
      instr <- readInstr bios
      execute instr
      liftIO $ print instr

-- getCB :: Get Op
-- getCB = getWord8 >>= \case
--   0x7c -> op "BIT 7,H"
--   0x11 -> op "RL C"
--   unknown -> op $ "CB/UNKNOWN: " <> show unknown

-- getInstruction :: Get Op
-- getInstruction = getWord8 >>= \case
--   0x04 -> op "INC B"
--   0x05 -> op "DEC B"
--   0x06 -> do
--     _ <- getWord8
--     op "LD B,u8"
--   0x0c -> op "INC C"
--   0x0d -> op "DEC C"
--   0x0e -> do
--     _ <- getWord8
--     op "LD C,u8"
--   0x11 -> do
--     _ <- getWord16le
--     op "LD DE,u16"
--   0x13 -> op "INC DE"
--   0x15 -> op "DEC D"
--   0x16 -> do
--     _ <- getWord8
--     op "LD D,u8"
--   0x17 -> op "RLA"
--   0x1a -> op "LD A,(DE)"
--   0x1e -> do
--     _ <- getWord8
--     op "LD E,u8"
--   0x18 -> do
--     _ <- getWord8 -- i8
--     op "JR i8"
--   0x1d -> op "DEC E"
--   0x20 -> do
--     _ <- getWord8
--     op "JR NZ,i8"
--   0x21 -> do
--     _ <- getWord16le
--     op "LD HL,u16"
--   0x22 -> op "LD (HL+),A"
--   0x23 -> op "INC HL"
--   0x24 -> op "INC H"
--   0x2e -> do
--     _ <- getWord8
--     op "LD L,u8"
--   0x28 -> do
--     _ <- getWord8 -- i8
--     op "JR Z,i8"
--   0x31 -> do
--     _ <- getWord16le
--     op "LD SP,u16"
--   0x32 -> do op "LD (HL-),A"
--   0x3d -> op "DEC A"
--   0x3e -> do
--     _ <- getWord8
--     op "LD A,u8"
--   0x4f -> op "LD C,A"
--   0x56 -> op "LD D,(HL)"
--   0x57 -> op "LD D,A"
--   0x67 -> op "LD H,A"
--   0x77 -> op "LD (HL),A"
--   0x7b -> op "LD A,E"
--   0x7c -> op "LD A,H"
--   0x90 -> op "SUB A,B"
--   0xaf -> op "XOR A"
--   0xc1 -> op "POP BC"
--   0xc5 -> op "PUSH BC"
--   0xc9 -> op "RET"
--   0xcb -> getCB
--   0xcd -> do
--     _ <- getWord16le
--     op "CALL u16"
--   0xe0 -> do
--     _ <- getWord8
--     op "LD (FF00+u8,A)"
--   0xe2 ->
--     op "LD (FF00+C,A)"
--   0xea -> do
--     _ <- getWord16le
--     op "LD (u16),A"
--   0xf0 -> do
--     _ <- getWord8
--     op "LD A,(FF00+u8)"
--   0xfe -> do
--     _ <- getWord8
--     op "CP A,u8"
--   unknown -> op $ "UNKNOWN: " <> show unknown
