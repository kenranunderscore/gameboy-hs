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
    in r{b = b1, c = b2}

getDE :: Registers -> U16
getDE r = combineU8s r.d r.e

setDE :: Registers -> U16 -> Registers
setDE r n =
    let (b1, b2) = splitU16 n
    in r{d = b1, e = b2}

getHL :: Registers -> U16
getHL r = combineU8s r.h r.l

setHL :: Registers -> U16 -> Registers
setHL r n =
    let (b1, b2) = splitU16 n
    in r{h = b1, l = b2}

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
    | LD_HLminus_A
    | LD_C_u8 U8
    | LD_A_u8 U8
    | BIT_7_H
    | JR_NZ_i8 I8
    | XOR_A

instance Show Instr where
    show = \case
        LD_SP_u16 u16 -> "LD SP," <> toHex u16
        LD_HL_u16 u16 -> "LD HL," <> toHex u16
        LD_HLminus_A -> "LD (HL-),A"
        LD_C_u8 u8 -> "LD C," <> toHex u8
        LD_A_u8 u8 -> "LD A," <> toHex u8
        BIT_7_H -> "BIT 7,H"
        JR_NZ_i8 i8 -> "JR NZ," <> show i8
        XOR_A -> "XOR A"

fetchU8 :: Memory -> U16 -> U8
fetchU8 = (!)

fetchI8 :: Memory -> U16 -> I8
fetchI8 mem = fromIntegral . fetchU8 mem

fetchU16 :: Memory -> U16 -> U16
fetchU16 mem addr = do
    let
        b1 = mem ! (addr + 1) -- little Endian
        b2 = mem ! addr
    (fromIntegral b1 .<<. 8) .|. fromIntegral b2

fetch :: CPU m => Memory -> m Instr
fetch mem = do
    counter <- gets pc
    advance 1
    case mem ! counter of
        0x0e -> do
            let u8 = fetchU8 mem (counter + 1)
            advance 1
            pure $ LD_C_u8 u8
        0x20 -> do
            let i8 = fetchI8 mem (counter + 1)
            advance 1
            pure $ JR_NZ_i8 i8
        0x21 -> do
            let u16 = fetchU16 mem (counter + 1)
            advance 2
            pure $ LD_HL_u16 u16
        0x31 -> do
            let u16 = fetchU16 mem (counter + 1)
            advance 2
            pure $ LD_SP_u16 u16
        0x32 -> pure LD_HLminus_A
        0x3e -> do
            let u8 = fetchU8 mem (counter + 1)
            advance 1
            pure $ LD_A_u8 u8
        0xaf -> pure XOR_A
        0xcb -> fetchPrefixed mem
        b -> fail $ "unknown opcode: " <> toHex b

fetchPrefixed :: CPU m => Memory -> m Instr
fetchPrefixed mem = do
    byte <- (mem !) <$> gets pc
    advance 1
    case byte of
        0x7c -> pure BIT_7_H
        s -> fail $ toHex s

execute :: (MonadIO m, CPU m) => Instr -> m ()
execute instr = do
    registers <- get
    let newRegisters =
            case instr of
                XOR_A -> registers{a = registers.a `xor` registers.a}
                LD_SP_u16 u16 -> registers{sp = u16}
                LD_HL_u16 u16 -> setHL registers u16
                LD_HLminus_A -> setHL registers (fromIntegral registers.a - 1)
                LD_C_u8 u8 -> registers{c = u8}
                LD_A_u8 u8 -> registers{a = u8}
                BIT_7_H -> registers -- TODO test bit and set flags
                JR_NZ_i8 i8 -> registers -- TODO
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
