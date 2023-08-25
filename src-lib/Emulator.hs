{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Emulator where

import Control.Monad
import Control.Monad.State.Strict
import Data.Array ((!), (//))
import Data.Bits ((.&.), (.<<.), (.>>.), (.|.))
import qualified Data.Bits as Bits

import Memory

data Registers = Registers
    { a :: U8
    , b :: U8
    , c :: U8
    , d :: U8
    , e :: U8
    , h :: U8
    , l :: U8
    , -- TODO: benchmark later whether a simple Haskell value can be used here
      -- to make everything more readable
      f :: U8
    , pc :: U16
    , sp :: U16
    }
    deriving stock (Eq)

-- | Lower 4 bits of the F register.
data Flag = Zero | Negative | HalfCarry | Carry
    deriving (Show)

flagBit :: Flag -> Int
flagBit = \case
    Zero -> 7
    Negative -> 6
    HalfCarry -> 5
    Carry -> 4

modifyFlag' :: Flag -> Bool -> Registers -> Registers
modifyFlag' flag on r =
    let change = if on then Bits.setBit else Bits.clearBit
    in r{f = change r.f (flagBit flag)}

setFlag' :: Flag -> Registers -> Registers
setFlag' flag r = modifyFlag' flag True r

clearFlag' :: Flag -> Registers -> Registers
clearFlag' flag r = modifyFlag' flag False r

hasFlag' :: Flag -> Registers -> Bool
hasFlag' flag r = Bits.testBit r.f (flagBit flag)

combineU8s :: U8 -> U8 -> U16
combineU8s b1 b2 = (fromIntegral b1 .<<. 8) .|. fromIntegral b2

splitU16 :: U16 -> (U8, U8)
splitU16 b = (fromIntegral (b .>>. 8), fromIntegral (b .&. 0xff))

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

data CPUState = CPUState
    { registers :: Registers
    , memory :: Memory
    }
    deriving stock (Show)

programCounter :: CPUState -> U16
programCounter = (.registers.pc)

mkInitialState :: Memory -> CPUState
mkInitialState mem = CPUState initialRegisters mem
  where
    initialRegisters =
        Registers
            { a = 0
            , b = 0
            , c = 0
            , d = 0
            , e = 0
            , h = 0
            , l = 0
            , f = 0
            , pc = 0
            , sp = 0xfffe
            }

type CPU m = (MonadState CPUState m, MonadFail m)

advance :: CPU m => U16 -> m ()
advance n =
    modify'
        (\s -> s{registers = s.registers{pc = s.registers.pc + n}})

data Instr
    = LD_SP_u16 U16 -- TODO: replace flat instructions with a tree
    | LD_HL_u16 U16
    | LD_HLminus_A
    | LD_A_u8 U8
    | LD_A_derefDE
    | LD_B_A
    | LD_B_u8 U8
    | LD_C_A
    | LD_C_u8 U8
    | LD_DE_u16 U16
    | LD_FF00plusC_A
    | LD_FF00plusU8_A U8
    | LD_derefHL_A
    | BIT_7_H
    | JR_NZ_i8 I8
    | XOR_A
    | INC_C
    | CALL U16
    | PUSH_BC

instance Show Instr where
    show = \case
        LD_SP_u16 u16 -> "LD SP," <> toHex u16
        LD_HL_u16 u16 -> "LD HL," <> toHex u16
        LD_HLminus_A -> "LD (HL-),A"
        LD_A_u8 u8 -> "LD A," <> toHex u8
        LD_A_derefDE -> "LD A,(DE)"
        LD_B_A -> "LD B,A"
        LD_B_u8 u8 -> "LD B," <> toHex u8
        LD_C_A -> "LD C,A"
        LD_C_u8 u8 -> "LD C," <> toHex u8
        LD_DE_u16 u16 -> "LD DE," <> toHex u16
        LD_FF00plusC_A -> "LD ($ff00+C,A)"
        LD_FF00plusU8_A u8 -> "LD ($ff00+$" <> toHex u8 <> ",A)"
        LD_derefHL_A -> "LD (HL),A"
        BIT_7_H -> "BIT 7,H"
        JR_NZ_i8 i8 -> "JR NZ," <> show i8
        XOR_A -> "XOR A"
        INC_C -> "INC C"
        CALL u16 -> "CALL " <> toHex u16
        PUSH_BC -> "PUSH BC"

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

fetch :: CPU m => m Instr
fetch = do
    counter <- gets programCounter
    mem <- gets memory
    advance 1
    case mem ! counter of
        0x06 -> do
            let u8 = fetchU8 mem (counter + 1)
            advance 1
            pure $ LD_B_u8 u8
        0x0c -> pure INC_C
        0x0e -> do
            let u8 = fetchU8 mem (counter + 1)
            advance 1
            pure $ LD_C_u8 u8
        0x11 -> do
            let u16 = fetchU16 mem (counter + 1)
            advance 2
            pure $ LD_DE_u16 u16
        0x1a -> pure LD_A_derefDE
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
        0x47 -> pure LD_B_A
        0x4f -> pure LD_C_A
        0x77 -> pure LD_derefHL_A
        0xaf -> pure XOR_A
        0xc5 -> pure PUSH_BC
        0xcb -> fetchPrefixed mem
        0xcd -> do
            let u16 = fetchU16 mem (counter + 1)
            advance 2
            pure $ CALL u16
        0xe0 -> do
            let u8 = fetchU8 mem (counter + 1)
            advance 1
            pure $ LD_FF00plusU8_A u8
        0xe2 -> pure LD_FF00plusC_A
        b -> fail $ "unknown opcode: " <> toHex b

fetchPrefixed :: CPU m => Memory -> m Instr
fetchPrefixed mem = do
    byte <- (mem !) <$> gets (.registers.pc)
    advance 1
    case byte of
        0x7c -> pure BIT_7_H
        s -> fail $ "unknown prefixed byte: " <> toHex s

push :: CPU m => U16 -> m ()
push u16 = modify' $ \s ->
    let
        sp' = s.registers.sp - 2
        (hi, lo) = splitU16 u16
    in
        s
            { registers = s.registers{sp = sp'}
            , memory = s.memory // [(sp', lo), (sp' + 1, hi)] -- TODO: check
            }

execute :: (MonadIO m, CPU m) => Instr -> m ()
execute = \case
    XOR_A -> modify' $ \s ->
        s{registers = s.registers{a = 0}}
    LD_SP_u16 u16 -> modify' $ \s ->
        s{registers = s.registers{sp = u16}}
    LD_HL_u16 u16 -> modify' $ \s ->
        s{registers = setHL s.registers u16}
    LD_HLminus_A -> modify' $ \s ->
        let hl = getHL s.registers
        in s{registers = setHL s.registers (hl - 1), memory = s.memory // [(hl, s.registers.a)]}
    LD_A_derefDE -> modify' $ \s ->
        s{registers = s.registers{a = s.memory ! getDE s.registers}}
    LD_A_u8 u8 -> modify' $ \s ->
        s{registers = s.registers{a = u8}}
    LD_B_A -> modify' $ \s ->
        s{registers = s.registers{b = s.registers.a}}
    LD_B_u8 u8 -> modify' $ \s ->
        s{registers = s.registers{b = u8}}
    LD_C_A -> modify' $ \s ->
        s{registers = s.registers{c = s.registers.a}}
    LD_C_u8 u8 -> modify' $ \s ->
        s{registers = s.registers{c = u8}}
    LD_DE_u16 u16 -> modify' $ \s ->
        s{registers = setDE s.registers u16}
    LD_FF00plusC_A -> modify' $ \s ->
        s{memory = s.memory // [(0xff00 + fromIntegral s.registers.c, s.registers.a)]}
    LD_FF00plusU8_A u8 -> modify' $ \s ->
        s{memory = s.memory // [(0xff00 + fromIntegral u8, s.registers.a)]}
    LD_derefHL_A -> modify' $ \s ->
        let hl = getHL s.registers in s{memory = s.memory // [(hl, s.registers.a)]}
    BIT_7_H -> modify' $ \s ->
        let
            r' = setFlag' HalfCarry $ clearFlag' Negative s.registers
            bitIsSet = Bits.testBit s.registers.h 7
        in
            s{registers = modifyFlag' Zero (not bitIsSet) r'}
    JR_NZ_i8 i8 -> modify' $ \s ->
        if not $ hasFlag' Zero s.registers
            then s{registers = s.registers{pc = s.registers.pc + fromIntegral i8}}
            else s
    INC_C -> modify' $ \s ->
        s{registers = s.registers{c = s.registers.c + 1}}
    CALL u16 -> do
        push u16
        modify' $ \s -> s{registers = s.registers{pc = u16}}
    PUSH_BC -> do
        bc <- gets (getBC . registers)
        push bc

run :: IO ()
run = do
    finalRegisters <- execStateT startup (mkInitialState bios)
    putStrLn "done"
    print finalRegisters

startup :: (MonadIO m, CPU m) => m ()
startup = loop
  where
    loop = forever $ do
        instr <- fetch
        execute instr
        pc <- gets programCounter
        liftIO . putStrLn $ toHex pc <> " : " <> show instr
