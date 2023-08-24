{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Emulator where

import Control.Monad
import Control.Monad.State.Strict
import Data.Array ((!))
import Data.Bits

import Memory

data Registers = Registers
    { a :: U8
    , f :: U8
    , b :: U8
    , c :: U8
    , d :: U8
    , e :: U8
    , h :: U8
    , l :: U8
    , pc :: U16
    , sp :: U16
    }
    deriving stock (Eq)

combineU8s :: U8 -> U8 -> U16
combineU8s b1 b2 = (fromIntegral b1 .<<. 8) .|. fromIntegral b2

splitU16 :: U16 -> (U8, U8)
splitU16 b = (fromIntegral (b .>>. 8), fromIntegral (b .&. 0xff))

-- skip AF for now, as I haven't yet understood it, and I think it's not used
-- similarly to the others

getBC :: Registers -> U16
getBC r = combineU8s r.b r.c

setBC :: Registers -> U16 -> Registers
setBC r n =
    let (b1, b2) = splitU16 n
    in r{b = b1, c = b2} -- TODO: check

getDE :: Registers -> U16
getDE r = combineU8s r.d r.e

setDE :: Registers -> U16 -> Registers
setDE r n =
    let (b1, b2) = splitU16 n
    in r{d = b1, e = b2} -- TODO: check

getHL :: Registers -> U16
getHL r = combineU8s r.h r.l

setHL :: Registers -> U16 -> Registers
setHL r n =
    let (b1, b2) = splitU16 n
    in r{h = b1, l = b2} -- TODO: check

{- FOURMOLU_DISABLE -}
instance Show Registers where
    show r = mconcat
        [ "A  = " , toHex r.a
        , "\nF  = " , toHex r.f
        , "\nB  = " , toHex r.b
        , "\nC  = " , toHex r.c , "    BC = " , toHex (getBC r)
        , "\nD  = " , toHex r.d
        , "\nE  = " , toHex r.e , "    DE = " , toHex (getDE r)
        , "\nH  = " , toHex r.h
        , "\nL  = " , toHex r.l , "    HL = " , toHex (getHL r)
        , "\nPC = " , toHex r.pc
        , "\nSP = " , toHex r.sp
        ]
{- FOURMOLU_ENABLE -}

initialRegisters :: Registers
initialRegisters = Registers 0 0 0 0 0 0 0 0 0 0

type CPU m = (MonadState Registers m, MonadFail m)

advance :: CPU m => U16 -> m ()
advance n = do
    registers <- get
    put $ registers{pc = registers.pc + n}

data Instr
    = LD_SP_u16 U16 -- TODO: replace flat instructions with a tree
    | LD_HL_u16 U16
    | XOR_A

instance Show Instr where
    show = \case
        LD_SP_u16 u16 -> "LD SP," <> toHex u16
        LD_HL_u16 u16 -> "LD HL," <> toHex u16
        XOR_A -> "XOR A"

fetchU16 :: Memory -> U16 -> U16
fetchU16 mem addr = do
    let
        b1 = mem ! (addr + 1) -- little Endian
        b2 = mem ! addr
    (fromIntegral b1 .<<. 8) .|. fromIntegral b2

fetch :: CPU m => Memory -> m Instr
fetch mem = do
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
        b -> fail $ "unknown opcode: " <> showU8 b

execute :: (MonadIO m, CPU m) => Instr -> m ()
execute instr = do
    registers <- get
    let newRegisters =
            case instr of
                XOR_A -> registers{a = registers.a `xor` registers.a}
                LD_SP_u16 u16 -> registers{sp = u16}
                LD_HL_u16 u16 -> setHL registers u16
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
        instr <- fetch bios
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
--     _ <- getU16le
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
--     _ <- getU16le
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
--     _ <- getU16le
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
--     _ <- getU16le
--     op "CALL u16"
--   0xe0 -> do
--     _ <- getWord8
--     op "LD (FF00+u8,A)"
--   0xe2 ->
--     op "LD (FF00+C,A)"
--   0xea -> do
--     _ <- getU16le
--     op "LD (u16),A"
--   0xf0 -> do
--     _ <- getWord8
--     op "LD A,(FF00+u8)"
--   0xfe -> do
--     _ <- getWord8
--     op "CP A,u8"
--   unknown -> op $ "UNKNOWN: " <> show unknown
