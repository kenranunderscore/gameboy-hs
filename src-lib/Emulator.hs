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
import Debug.Trace
import qualified Numeric
import qualified System.Environment as Environment

import Memory

toHex :: Integral a => a -> String
toHex n = "$" <> flip Numeric.showHex mempty n

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
combineU8s hi lo = (fromIntegral hi .<<. 8) .|. fromIntegral lo

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

stackPointer :: CPUState -> U16
stackPointer = (.registers.sp)

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
            , pc = 0x100 -- start without BIOS for now
            , sp = 0xfffe
            }

type CPU m = MonadState CPUState m

advance :: CPU m => U16 -> m ()
advance n =
    modify'
        (\s -> s{registers = s.registers{pc = s.registers.pc + n}})

data Instr
    = LD_SP_u16 U16 -- TODO: replace flat instructions with a tree
    | LD_HL_u16 U16
    | LD_A_u8 U8
    | LD_A_derefDE
    | LD_A_FF00plusU8 U8
    | LD_B_A
    | LD_B_u8 U8
    | LD_C_A
    | LD_C_u8 U8
    | LD_DE_u16 U16
    | LD_FF00plusC_A
    | LD_FF00plusU8_A U8
    | LD_derefHL_A
    | LD_HLminus_A
    | LD_HLplus_A
    | BIT_7_H
    | JP_u16 U16
    | JR_NZ_i8 I8
    | XOR_A
    | INC_C
    | INC_HL
    | DEC_B
    | DEC_C
    | CALL U16
    | RET
    | PUSH_BC
    | POP_BC
    | RLA
    | RL_C
    | DI
    | NOP

instance Show Instr where
    show = \case
        LD_SP_u16 u16 -> "LD SP," <> toHex u16
        LD_HL_u16 u16 -> "LD HL," <> toHex u16
        LD_derefHL_A -> "LD (HL),A"
        LD_HLminus_A -> "LD (HL-),A"
        LD_HLplus_A -> "LD (HL+),A"
        LD_A_u8 u8 -> "LD A," <> toHex u8
        LD_A_derefDE -> "LD A,(DE)"
        LD_A_FF00plusU8 u8 -> "LD A,($ff00+" <> toHex u8 <> ")"
        LD_B_A -> "LD B,A"
        LD_B_u8 u8 -> "LD B," <> toHex u8
        LD_C_A -> "LD C,A"
        LD_C_u8 u8 -> "LD C," <> toHex u8
        LD_DE_u16 u16 -> "LD DE," <> toHex u16
        LD_FF00plusC_A -> "LD ($ff00+C,A)"
        LD_FF00plusU8_A u8 -> "LD ($ff00+" <> toHex u8 <> "),A"
        BIT_7_H -> "BIT 7,H"
        JR_NZ_i8 i8 -> "JR NZ," <> show i8
        JP_u16 u16 -> "JP " <> toHex u16
        XOR_A -> "XOR A"
        INC_C -> "INC C"
        INC_HL -> "INC HL"
        DEC_B -> "DEC B"
        DEC_C -> "DEC C"
        CALL u16 -> "CALL " <> toHex u16
        RET -> "RET"
        PUSH_BC -> "PUSH BC"
        POP_BC -> "POP BC"
        RLA -> "RLA"
        RL_C -> "RL C"
        DI -> "DI"
        NOP -> "NOP"

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
    res <- case mem ! counter of
        0 -> pure NOP
        0x05 -> pure DEC_B
        0x06 -> do
            let u8 = fetchU8 mem (counter + 1)
            advance 1
            pure $ LD_B_u8 u8
        0x0c -> pure INC_C
        0x0d -> pure DEC_C
        0x0e -> do
            let u8 = fetchU8 mem (counter + 1)
            advance 1
            pure $ LD_C_u8 u8
        0x11 -> do
            let u16 = fetchU16 mem (counter + 1)
            advance 2
            pure $ LD_DE_u16 u16
        0x17 -> pure RLA
        0x1a -> pure LD_A_derefDE
        0x20 -> do
            let i8 = fetchI8 mem (counter + 1)
            advance 1
            pure $ JR_NZ_i8 i8
        0x21 -> do
            let u16 = fetchU16 mem (counter + 1)
            advance 2
            pure $ LD_HL_u16 u16
        0x22 -> pure LD_HLplus_A
        0x23 -> pure INC_HL
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
        0xc1 -> pure POP_BC
        0xc3 -> do
            let u16 = fetchU16 mem (counter + 1)
            advance 2
            pure $ JP_u16 u16
        0xc5 -> pure PUSH_BC
        0xc9 -> pure RET
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
        0xf0 -> do
            let u8 = fetchU8 mem (counter + 1)
            advance 1
            pure $ LD_A_FF00plusU8 u8
        0xf3 -> pure DI
        b -> error $ "unknown opcode: " <> toHex b
    pure res

fetchPrefixed :: CPU m => Memory -> m Instr
fetchPrefixed mem = do
    byte <- (mem !) <$> gets (.registers.pc)
    advance 1
    case byte of
        0x11 -> pure RL_C
        0x7c -> pure BIT_7_H
        s -> error $ "unknown prefixed byte: " <> toHex s

push :: CPU m => U16 -> m ()
push n = do
    -- TODO: correct SP?  Cinoop and very-lazy-boy decrement before writing to
    -- the new location, but wouldn't that prevent the last address from ever
    -- being used?
    modify' $ \s ->
        trace ("    pushed " <> toHex n) $
            s
                { registers = s.registers{sp = s.registers.sp - 2}
                , memory = s.memory // [(s.registers.sp - 2, lo), (s.registers.sp - 1, hi)]
                }
  where
    (hi, lo) = splitU16 n

pop :: CPU m => m U16
pop = do
    -- TODO: do I have to zero the popped memory location?
    s <- get
    put s{registers = s.registers{sp = s.registers.sp + 2}}
    let
        lo = s.memory ! s.registers.sp
        hi = s.memory ! (s.registers.sp + 1)
        n = combineU8s hi lo
    traceM $ "    popped " <> toHex n
    pure n

execute :: CPU m => Instr -> m ()
execute = \case
    NOP -> pure ()
    XOR_A -> modify' $ \s ->
        s{registers = s.registers{a = 0}}
    LD_SP_u16 u16 -> modify' $ \s ->
        s{registers = s.registers{sp = u16}}
    LD_HL_u16 u16 -> modify' $ \s ->
        s{registers = setHL s.registers u16}
    LD_HLminus_A -> modify' $ \s ->
        let hl = getHL s.registers
        in s
            { registers = setHL s.registers (hl - 1)
            , memory = s.memory // [(hl, s.registers.a)]
            }
    LD_HLplus_A -> modify' $ \s ->
        let hl = getHL s.registers
        in s
            { registers = setHL s.registers (hl + 1)
            , memory = s.memory // [(hl, s.registers.a)]
            }
    LD_A_derefDE -> modify' $ \s ->
        s{registers = s.registers{a = s.memory ! getDE s.registers}}
    LD_A_FF00plusU8 u8 -> modify' $ \s ->
        s{registers = s.registers{a = s.memory ! 0xff00 + u8}}
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
    JP_u16 u16 -> modify' $ \s ->
        s{registers = s.registers{pc = u16}}
    INC_C -> modify' $ \s ->
        s{registers = s.registers{c = s.registers.c + 1}}
    INC_HL -> modify' $ \s ->
        s{registers = setHL s.registers (getHL s.registers + 1)}
    DEC_B -> modify' $ \s ->
        let result = s.registers.b - 1
        in s
            { registers =
                modifyFlag' Zero (result == 0) $
                    setFlag' Negative $
                        modifyFlag' HalfCarry (s.registers.b .&. 0x0f == 0) $
                            s.registers{b = result}
            }
    DEC_C -> modify' $ \s ->
        let result = s.registers.c - 1
        in s
            { registers =
                modifyFlag' Zero (result == 0) $
                    setFlag' Negative $
                        modifyFlag' HalfCarry (s.registers.c .&. 0x0f == 0) $
                            s.registers{c = result}
            }
    CALL u16 -> do
        pc <- gets programCounter
        push (pc + 1)
        traceM $ "    CALL " <> toHex u16
        modify' $ \s -> s{registers = s.registers{pc = u16}}
    RET -> do
        addr <- pop
        traceM $ "    RET " <> toHex addr
        modify' $ \s -> s{registers = s.registers{pc = addr}}
    PUSH_BC -> do
        bc <- gets (getBC . registers)
        push bc
    POP_BC -> do
        u16 <- pop
        modify' $ \s -> s{registers = setBC s.registers u16}
    RLA -> modify' $ \s ->
        let
            carry = if hasFlag' Carry s.registers then 1 else 0
            carry' = Bits.testBit s.registers.a 7
            a' = Bits.shiftL s.registers.a 1 + carry
        in
            s
                { registers =
                    modifyFlag' Carry carry' $
                        clearFlag' Zero $ -- TODO: check: some do this, but manual says it changes
                            clearFlag' Negative $
                                clearFlag' HalfCarry $
                                    s.registers{a = a'}
                }
    RL_C -> modify' $ \s ->
        let
            carry = if hasFlag' Carry s.registers then 1 else 0
            carry' = Bits.testBit s.registers.c 7
            c' = Bits.shiftL s.registers.c 1 + carry
        in
            s
                { registers =
                    modifyFlag' Carry carry' $
                        modifyFlag' Zero (c' == 0) $
                            clearFlag' Negative $
                                clearFlag' HalfCarry $
                                    s.registers{c = c'}
                }
    DI -> pure () -- TODO: disable interrupts

run :: IO ()
run = do
    args <- Environment.getArgs
    case args of
        [] -> fail "need path to ROM as first argument"
        (cartridgePath : _) -> do
            mem <- loadCartridge cartridgePath
            finalRegisters <- execStateT startup (mkInitialState mem)
            putStrLn "done"
            print finalRegisters

startup :: (MonadIO m, CPU m) => m ()
startup = loop
  where
    loop = forever $ do
        s <- get
        instr <- fetch
        execute instr
        liftIO $ putStrLn $ toHex s.registers.pc <> " :  " <> show instr
