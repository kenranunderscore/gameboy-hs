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

combineBytes :: U8 -> U8 -> U16
combineBytes hi lo = (fromIntegral hi .<<. 8) .|. fromIntegral lo

splitIntoBytes :: U16 -> (U8, U8)
splitIntoBytes n = (fromIntegral (n .>>. 8), fromIntegral (n .&. 0xff))

combineRegisters :: Lens' Registers U8 -> Lens' Registers U8 -> Lens' Registers U16
combineRegisters hiL loL =
    lens
        (\r -> combineBytes (view hiL r) (view loL r))
        (\r n -> let (hi, lo) = splitIntoBytes n in r & hiL .~ hi & loL .~ lo)

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

data TargetRegister = A | B | C | D | E | H | L
    deriving stock (Show)

targetL :: TargetRegister -> Lens' Registers U8
targetL = \case
    A -> a
    B -> b
    C -> c
    D -> d
    E -> e
    H -> h
    L -> l

data TargetRegister16 = BC | DE | HL | SP
    deriving stock (Show)

target16L :: TargetRegister16 -> Lens' Registers U16
target16L = \case
    BC -> bc
    DE -> de
    HL -> hl
    SP -> sp

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
    | INC TargetRegister
    | INC_derefHL
    | INC16 TargetRegister16
    | DEC TargetRegister
    | DEC_derefHL
    | DEC16 TargetRegister16
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
        INC r -> "INC " <> show r
        INC_derefHL -> "INC (HL)"
        INC16 r -> "INC " <> show r
        DEC r -> "DEC " <> show r
        DEC_derefHL -> "DEC (HL)"
        DEC16 r -> "DEC " <> show r
        CALL n -> "CALL " <> toHex n
        RET -> "RET"
        PUSH_BC -> "PUSH BC"
        POP_BC -> "POP BC"
        RLA -> "RLA"
        RL_C -> "RL C"
        DI -> "DI"
        NOP -> "NOP"
        CP_A_u8 n -> "CP A," <> toHex n

fetchByte :: Memory -> U16 -> U8
fetchByte = (!)

fetchByteM :: CPU m => m U8
fetchByteM = do
    s <- get
    advance 1
    pure $ fetchByte (view memory s) (view programCounter s)

fetchI8M :: CPU m => m I8
fetchI8M = fromIntegral <$> fetchByteM

fetchU16 :: Memory -> U16 -> U16
fetchU16 mem addr = do
    let
        hi = mem ! (addr + 1) -- little Endian
        lo = mem ! addr
    (fromIntegral hi .<<. 8) .|. fromIntegral lo

fetchU16M :: CPU m => m U16
fetchU16M = do
    s <- get
    advance 2
    pure $ fetchU16 (view memory s) (view programCounter s)

fetch :: CPU m => m Instr
fetch = do
    counter <- gets (view programCounter)
    mem <- gets (view memory)
    advance 1
    case mem ! counter of
        0 -> pure NOP
        0x03 -> pure $ INC16 BC
        0x04 -> pure $ INC B
        0x05 -> pure $ DEC B
        0x06 -> LD_B_u8 <$> fetchByteM
        0x0b -> pure $ DEC16 BC
        0x0c -> pure $ INC C
        0x0d -> pure $ DEC C
        0x0e -> LD_C_u8 <$> fetchByteM
        0x11 -> LD_DE_u16 <$> fetchU16M
        0x13 -> pure $ INC16 DE
        0x14 -> pure $ INC D
        0x15 -> pure $ DEC D
        0x17 -> pure RLA
        0x1a -> pure LD_A_derefDE
        0x1b -> pure $ DEC16 DE
        0x1c -> pure $ INC E
        0x1d -> pure $ DEC E
        0x20 -> JR_NZ_i8 <$> fetchI8M
        0x21 -> LD_HL_u16 <$> fetchU16M
        0x22 -> pure LD_HLplus_A
        0x23 -> pure $ INC16 HL
        0x24 -> pure $ INC H
        0x25 -> pure $ DEC H
        0x2b -> pure $ DEC16 HL
        0x2c -> pure $ INC L
        0x2d -> pure $ DEC L
        0x31 -> LD_SP_u16 <$> fetchU16M
        0x32 -> pure LD_HLminus_A
        0x33 -> pure $ INC16 SP
        0x34 -> pure INC_derefHL
        0x35 -> pure DEC_derefHL
        0x3b -> pure $ DEC16 SP
        0x3c -> pure $ INC A
        0x3d -> pure $ DEC A
        0x3e -> LD_A_u8 <$> fetchByteM
        0x47 -> pure LD_B_A
        0x4f -> pure LD_C_A
        0x77 -> pure LD_derefHL_A
        0xaf -> pure XOR_A
        0xc1 -> pure POP_BC
        0xc3 -> JP_u16 <$> fetchU16M
        0xc5 -> pure PUSH_BC
        0xc9 -> pure RET
        0xcb -> fetchPrefixed mem
        0xcd -> CALL <$> fetchU16M
        0xe0 -> LD_FF00plusU8_A <$> fetchByteM
        0xe2 -> pure LD_FF00plusC_A
        0xf0 -> LD_A_FF00plusU8 <$> fetchByteM
        0xf3 -> pure DI
        0xfe -> CP_A_u8 <$> fetchByteM
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
    (hi, lo) = splitIntoBytes n

pop :: CPU m => m U16
pop = do
    -- TODO: do I have to zero the popped memory location?
    s <- get
    put (s & registers % sp %~ (+ 2))
    let
        lo = view memory s ! view stackPointer s
        hi = view memory s ! (view stackPointer s + 1)
        n = combineBytes hi lo
    traceM $ "    popped " <> toHex n
    pure n

dec :: CPU m => Lens' Registers U8 -> m ()
dec reg = modify' $ \s ->
    let
        old = s ^. registers % reg
        result = old - 1
    in
        s
            { _registers =
                modifyFlag' Zero (result == 0) $
                    setFlag' Negative $
                        modifyFlag' HalfCarry (old .&. 0x0f == 0) $
                            (s._registers & reg .~ result)
            }

inc :: CPU m => Lens' Registers U8 -> m ()
inc reg = modify' $ \s ->
    let
        old = s ^. registers % reg
        result = old + 1
    in
        s
            { _registers =
                modifyFlag' Zero (result == 0) $
                    clearFlag' Negative $
                        modifyFlag' HalfCarry (old .&. 0x0f == 0x0f) $
                            (s._registers & reg .~ result)
            }

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
        let
            c' = s ^. registers % c
            a' = s ^. registers % a
        in
            s & memory %~ (// [(0xff00 + fromIntegral c', a')])
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
    INC r ->
        inc (targetL r)
    INC_derefHL -> modify' $ \s ->
        let
            p = s ^. registers % hl
            val = (view memory s ! p) + 1
        in
            s & memory %~ (// [(p, val)])
    INC16 r ->
        modifying' (registers % (target16L r)) (+ 1)
    DEC r ->
        dec (targetL r)
    DEC_derefHL -> modify' $ \s ->
        let
            p = s ^. registers % hl
            val = (view memory s ! p) - 1
        in
            s
                & memory %~ (// [(p, val)])
                & registers
                    .~ ( modifyFlag' Zero (val == 0) $
                            setFlag' Negative $
                                modifyFlag' HalfCarry ((val + 1) .&. 0x0f == 0) $
                                    view registers s
                       )
    DEC16 r ->
        modifying' (registers % (target16L r)) (\n -> n - 1)
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
    DI ->
        pure () -- TODO: disable interrupts
    CP_A_u8 n -> modify' $ \s ->
        let
            r' = setFlag' Negative (view registers s)
            val = s ^. registers % a
        in
            s
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
