{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Emulator where

import Control.Monad
import Control.Monad.State.Strict
import Data.Array ((!), (//))
import Data.Bits ((.&.), (.<<.), (.>>.), (.|.))
import qualified Data.Bits as Bits
import Debug.Trace
import qualified Numeric
import Optics
import qualified System.Environment as Environment

import Memory

toHex :: Integral a => a -> String
toHex n = "$" <> flip Numeric.showHex mempty n

data Registers = Registers
    { _a :: U8
    , _b :: U8
    , _c :: U8
    , _d :: U8
    , _e :: U8
    , _h :: U8
    , _l :: U8
    , -- TODO: benchmark later whether a simple Haskell value can be used here
      -- to make everything more readable
      _f :: U8
    , _pc :: U16
    , _sp :: U16
    }
    deriving stock (Eq)

makeLenses ''Registers

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
    let
        change = if on then Bits.setBit else Bits.clearBit
        bit = flagBit flag
    in
        r & f %~ flip change bit

setFlag' :: Flag -> Registers -> Registers
setFlag' flag r = modifyFlag' flag True r

clearFlag' :: Flag -> Registers -> Registers
clearFlag' flag r = modifyFlag' flag False r

hasFlag' :: Flag -> Registers -> Bool
hasFlag' flag r = Bits.testBit r._f (flagBit flag)

combineU8s :: U8 -> U8 -> U16
combineU8s hi lo = (fromIntegral hi .<<. 8) .|. fromIntegral lo

splitU16 :: U16 -> (U8, U8)
splitU16 n = (fromIntegral (n .>>. 8), fromIntegral (n .&. 0xff))

combineRegisters :: Lens' Registers U8 -> Lens' Registers U8 -> Lens' Registers U16
combineRegisters hiL loL =
    lens
        (\r -> combineU8s (view hiL r) (view loL r))
        (\r n -> let (hi, lo) = splitU16 n in r & hiL .~ hi & loL .~ lo)

bc :: Lens' Registers U16
bc = combineRegisters b c

de :: Lens' Registers U16
de = combineRegisters d e

hl :: Lens' Registers U16
hl = combineRegisters h l

{- FOURMOLU_DISABLE -}
instance Show Registers where
    show r = mconcat
        [ "A  = " , toHex (view a r)
        , "\nF  = " , toHex (view f r)
        , "\nB  = " , toHex (view b r)
        , "\nC  = " , toHex (view c r) , "    BC = " , toHex (view bc r)
        , "\nD  = " , toHex (view d r)
        , "\nE  = " , toHex (view e r) , "    DE = " , toHex (view de r)
        , "\nH  = " , toHex (view h r)
        , "\nL  = " , toHex (view l r) , "    HL = " , toHex (view hl r)
        , "\nPC = " , toHex (view pc r)
        , "\nSP = " , toHex (view sp r)
        ]
{- FOURMOLU_ENABLE -}

data CPUState = CPUState
    { _registers :: Registers
    , _memory :: Memory
    }
    deriving stock (Show)

makeLenses ''CPUState

programCounter :: Lens' CPUState U16
programCounter = registers % pc

stackPointer :: Lens' CPUState U16
stackPointer = registers % sp

mkInitialState :: Memory -> CPUState
mkInitialState mem = CPUState initialRegisters mem
  where
    initialRegisters =
        Registers
            { _a = 0
            , _b = 0
            , _c = 0
            , _d = 0
            , _e = 0
            , _h = 0
            , _l = 0
            , _f = 0
            , _pc = 0x100 -- start without BIOS for now
            , _sp = 0xfffe
            }

type CPU m = MonadState CPUState m

advance :: CPU m => U16 -> m ()
advance n = modifying' programCounter (+ n)

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
    | CP_A_u8 U8

instance Show Instr where
    show = \case
        LD_SP_u16 n -> "LD SP," <> toHex n
        LD_HL_u16 n -> "LD HL," <> toHex n
        LD_derefHL_A -> "LD (HL),A"
        LD_HLminus_A -> "LD (HL-),A"
        LD_HLplus_A -> "LD (HL+),A"
        LD_A_u8 n -> "LD A," <> toHex n
        LD_A_derefDE -> "LD A,(DE)"
        LD_A_FF00plusU8 n -> "LD A,($ff00+" <> toHex n <> ")"
        LD_B_A -> "LD B,A"
        LD_B_u8 n -> "LD B," <> toHex n
        LD_C_A -> "LD C,A"
        LD_C_u8 n -> "LD C," <> toHex n
        LD_DE_u16 n -> "LD DE," <> toHex n
        LD_FF00plusC_A -> "LD ($ff00+C,A)"
        LD_FF00plusU8_A n -> "LD ($ff00+" <> toHex n <> "),A"
        BIT_7_H -> "BIT 7,H"
        JR_NZ_i8 n -> "JR NZ," <> show n
        JP_u16 n -> "JP " <> toHex n
        XOR_A -> "XOR A"
        INC_C -> "INC C"
        INC_HL -> "INC HL"
        DEC_B -> "DEC B"
        DEC_C -> "DEC C"
        CALL n -> "CALL " <> toHex n
        RET -> "RET"
        PUSH_BC -> "PUSH BC"
        POP_BC -> "POP BC"
        RLA -> "RLA"
        RL_C -> "RL C"
        DI -> "DI"
        NOP -> "NOP"
        CP_A_u8 n -> "CP A," <> toHex n

fetchU8 :: Memory -> U16 -> U8
fetchU8 = (!)

fetchI8 :: Memory -> U16 -> I8
fetchI8 mem = fromIntegral . fetchU8 mem

fetchU16 :: Memory -> U16 -> U16
fetchU16 mem addr = do
    let
        hi = mem ! (addr + 1) -- little Endian
        lo = mem ! addr
    (fromIntegral hi .<<. 8) .|. fromIntegral lo

fetch :: CPU m => m Instr
fetch = do
    counter <- gets (view programCounter)
    mem <- gets (view memory)
    advance 1
    case mem ! counter of
        0 -> pure NOP
        0x05 -> pure DEC_B
        0x06 -> do
            let n = fetchU8 mem (counter + 1)
            advance 1
            pure $ LD_B_u8 n
        0x0c -> pure INC_C
        0x0d -> pure DEC_C
        0x0e -> do
            let n = fetchU8 mem (counter + 1)
            advance 1
            pure $ LD_C_u8 n
        0x11 -> do
            let n = fetchU16 mem (counter + 1)
            advance 2
            pure $ LD_DE_u16 n
        0x17 -> pure RLA
        0x1a -> pure LD_A_derefDE
        0x20 -> do
            let n = fetchI8 mem (counter + 1)
            advance 1
            pure $ JR_NZ_i8 n
        0x21 -> do
            let n = fetchU16 mem (counter + 1)
            advance 2
            pure $ LD_HL_u16 n
        0x22 -> pure LD_HLplus_A
        0x23 -> pure INC_HL
        0x31 -> do
            let n = fetchU16 mem (counter + 1)
            advance 2
            pure $ LD_SP_u16 n
        0x32 -> pure LD_HLminus_A
        0x3e -> do
            let n = fetchU8 mem (counter + 1)
            advance 1
            pure $ LD_A_u8 n
        0x47 -> pure LD_B_A
        0x4f -> pure LD_C_A
        0x77 -> pure LD_derefHL_A
        0xaf -> pure XOR_A
        0xc1 -> pure POP_BC
        0xc3 -> do
            let n = fetchU16 mem (counter + 1)
            advance 2
            pure $ JP_u16 n
        0xc5 -> pure PUSH_BC
        0xc9 -> pure RET
        0xcb -> fetchPrefixed mem
        0xcd -> do
            let n = fetchU16 mem (counter + 1)
            advance 2
            pure $ CALL n
        0xe0 -> do
            let n = fetchU8 mem (counter + 1)
            advance 1
            pure $ LD_FF00plusU8_A n
        0xe2 -> pure LD_FF00plusC_A
        0xf0 -> do
            let n = fetchU8 mem (counter + 1)
            advance 1
            pure $ LD_A_FF00plusU8 n
        0xf3 -> pure DI
        0xfe -> do
            let n = fetchU8 mem (counter + 1)
            advance 1
            pure $ CP_A_u8 n
        unknown -> error $ "unknown opcode: " <> toHex unknown

fetchPrefixed :: CPU m => Memory -> m Instr
fetchPrefixed mem = do
    byte <- (mem !) <$> gets (view programCounter)
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
    modify'
        ( \s ->
            let curr = view stackPointer s
            in s
                & registers % sp %~ (\x -> x - 2)
                & memory %~ (// [(curr - 2, lo), (curr - 1, hi)])
        )
  where
    (hi, lo) = splitU16 n

pop :: CPU m => m U16
pop = do
    -- TODO: do I have to zero the popped memory location?
    s <- get
    put (s & registers % sp %~ (+ 2))
    let
        lo = view memory s ! view stackPointer s
        hi = view memory s ! (view stackPointer s + 1)
        n = combineU8s hi lo
    traceM $ "    popped " <> toHex n
    pure n

execute :: CPU m => Instr -> m ()
execute = \case
    NOP -> pure ()
    XOR_A ->
        assign' (registers % a) 0
    LD_SP_u16 n ->
        assign' (registers % sp) n
    LD_HL_u16 n ->
        assign' (registers % hl) n
    LD_HLminus_A -> modify' $ \s ->
        s
            & registers % hl %~ (\x -> x - 1)
            & memory %~ (// [(view (registers % hl) s, s ^. registers % a)])
    LD_HLplus_A -> modify' $ \s ->
        s
            & registers % hl %~ (+ 1)
            & memory %~ (// [(view (registers % hl) s, s ^. registers % a)])
    LD_A_derefDE -> modify' $ \s ->
        let addr = s ^. registers % de
        in s & registers % a .~ (view memory s ! addr)
    LD_A_FF00plusU8 n -> modify' $ \s ->
        s & registers % a .~ (view memory s ! 0xff00 + n)
    LD_A_u8 n ->
        assign' (registers % a) n
    LD_B_A -> modify' $ \s ->
        s & registers % b .~ (s ^. registers % a)
    LD_B_u8 n ->
        assign' (registers % b) n
    LD_C_A -> modify' $ \s ->
        s & registers % c .~ (s ^. registers % a)
    LD_C_u8 n ->
        assign' (registers % c) n
    LD_DE_u16 n ->
        assign' (registers % de) n
    LD_FF00plusC_A -> modify' $ \s ->
        let c' = s ^. registers % c
            a' = s ^. registers % a
        in s & memory %~ (// [(0xff00 + fromIntegral c', a')])
    LD_FF00plusU8_A n -> modify' $ \s ->
        s & memory %~ (// [(0xff00 + fromIntegral n, s ^. registers % a)])
    LD_derefHL_A -> modify' $ \s ->
        s & memory %~ (// [(s ^. registers % hl, s ^. registers % a)])
    BIT_7_H -> modify' $ \s ->
        let
            r' = setFlag' HalfCarry $ clearFlag' Negative s._registers
            bitIsSet = Bits.testBit (s ^. registers % h) 7
        in
            s & registers .~ modifyFlag' Zero (not bitIsSet) r'
    JR_NZ_i8 n -> modify' $ \s ->
        if not $ hasFlag' Zero (view registers s)
            then s & programCounter %~ (+ fromIntegral n)
            else s
    JP_u16 n ->
        assign' (registers % pc) n
    INC_C ->
        modifying' (registers % c) (+ 1)
    INC_HL ->
        modifying' (registers % hl) (+ 1)
    DEC_B -> modify' $ \s ->
        let old = s ^. registers % b
            result = old - 1
        in s
            { _registers =
                modifyFlag' Zero (result == 0) $
                    setFlag' Negative $
                        modifyFlag' HalfCarry (old .&. 0x0f == 0) $
                            s._registers{_b = result}
            }
    DEC_C -> modify' $ \s ->
        let old = s ^. registers % c
            result = old - 1
        in s
            { _registers =
                modifyFlag' Zero (result == 0) $
                    setFlag' Negative $
                        modifyFlag' HalfCarry (old .&. 0x0f == 0) $
                            s._registers{_c = result}
            }
    CALL n -> do
        counter <- gets (view programCounter)
        push (counter + 1)
        assign' (registers % pc) n
    RET -> do
        addr <- pop
        assign' (registers % pc) addr
    PUSH_BC -> do
        n <- gets (view (registers % bc))
        push n
    POP_BC -> do
        n <- pop
        assign' (registers % bc) n
    RLA -> modify' $ \s ->
        let
            carry = if hasFlag' Carry (view registers s) then 1 else 0
            carry' = Bits.testBit (s ^. registers % a) 7
            a' = Bits.shiftL (s ^. registers % a) 1 + carry
        in
            s
                { _registers =
                    modifyFlag' Carry carry' $
                        clearFlag' Zero $ -- TODO: check: some do this, but manual says it changes
                            clearFlag' Negative $
                                clearFlag' HalfCarry $
                                    s._registers{_a = a'}
                }
    RL_C -> modify' $ \s ->
        let
            carry = if hasFlag' Carry (view registers s) then 1 else 0
            carry' = Bits.testBit (s ^. registers % c) 7
            c' = Bits.shiftL (s ^. registers % c) 1 + carry
        in
            s
                { _registers =
                    modifyFlag' Carry carry' $
                        modifyFlag' Zero (c' == 0) $
                            clearFlag' Negative $
                                clearFlag' HalfCarry $
                                    s._registers{_c = c'}
                }
    DI -> pure () -- TODO: disable interrupts
    CP_A_u8 n -> modify' $ \s ->
        let r' = setFlag' Negative (view registers s)
            val = s ^. registers % a
        in s
            { _registers =
                modifyFlag' Zero (val == n) $
                    modifyFlag' Carry (val < n) $
                        -- modifyFlag' HalfCarry () $ -- TODO: implement
                        r'
            }

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
        liftIO $ putStrLn $ toHex (view programCounter s) <> " :  " <> show instr
