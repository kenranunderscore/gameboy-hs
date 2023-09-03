{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module GameBoy.CPU where

import Control.Monad
import Control.Monad.State.Strict
import Data.Bits ((.&.))
import Data.Bits qualified as Bits
import Debug.Trace
import Optics

import GameBoy.BitStuff
import GameBoy.Memory

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

flag :: Flag -> Lens' Registers Bool
flag fl = f % bit (flagBit fl)

clearFlag :: Flag -> Registers -> Registers
clearFlag fl = set (flag fl) False

setFlag :: Flag -> Registers -> Registers
setFlag fl = set (flag fl) True

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
    , _memoryBus :: MemoryBus
    , _dividerCounter :: Int
    , _timerCounter :: Int
    , _masterInterruptEnable :: Bool
    }
    deriving stock (Show)

makeLenses ''CPUState

hasFlag :: Flag -> CPUState -> Bool
hasFlag fl = view (registers % flag fl)

programCounter :: Lens' CPUState U16
programCounter = registers % pc

stackPointer :: Lens' CPUState U16
stackPointer = registers % sp

mkInitialState :: MemoryBus -> CPUState
mkInitialState bus = CPUState initialRegisters bus 0 1024 True
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
    = LD_u16 TargetRegister16 U16
    | LD_A_derefDE
    | LD_A_FF00plusU8 U8
    | LD_u8 TargetRegister U8
    | LD_FF00plusC_A
    | LD_FF00plusU8_A U8
    | LD_derefHL_A
    | LD_HLminus_A
    | LD_HLplus_A
    | LD TargetRegister TargetRegister
    | BIT Int TargetRegister
    | BIT_n_derefHL Int
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
    | EI
    | NOP
    | CP_A_u8 U8

instance Show Instr where
    show = \case
        LD_u16 rr n -> "LD " <> show rr <> "," <> toHex n
        LD_derefHL_A -> "LD (HL),A"
        LD_HLminus_A -> "LD (HL-),A"
        LD_HLplus_A -> "LD (HL+),A"
        LD_A_derefDE -> "LD A,(DE)"
        LD_A_FF00plusU8 n -> "LD A,($ff00+" <> toHex n <> ")"
        LD r r' -> "LD " <> show r <> "," <> show r'
        LD_u8 r n -> "LD " <> show r <> "," <> toHex n
        LD_FF00plusC_A -> "LD ($ff00+C,A)"
        LD_FF00plusU8_A n -> "LD ($ff00+" <> toHex n <> "),A"
        BIT n r -> "BIT " <> show n <> "," <> show r
        BIT_n_derefHL n -> "BIT " <> show n <> ",(HL)"
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
        EI -> "EI"
        NOP -> "NOP"
        CP_A_u8 n -> "CP A," <> toHex n

fetchByteM :: CPU m => m U8
fetchByteM = do
    s <- get
    advance 1
    pure $ readByte (view memoryBus s) (view programCounter s)

fetchI8M :: CPU m => m I8
fetchI8M = do
    fromIntegral <$> fetchByteM

fetchU16M :: CPU m => m U16
fetchU16M = do
    s <- get
    advance 2
    pure $ readU16 (view memoryBus s) (view programCounter s)

fetch :: CPU m => m Instr
fetch = do
    s <- get
    let
        counter = view programCounter s
        bus = view memoryBus s
    advance 1
    case readByte bus counter of
        0 -> pure NOP
        0x01 -> LD_u16 BC <$> fetchU16M
        0x03 -> pure $ INC16 BC
        0x04 -> pure $ INC B
        0x05 -> pure $ DEC B
        0x06 -> LD_u8 B <$> fetchByteM
        0x0b -> pure $ DEC16 BC
        0x0c -> pure $ INC C
        0x0d -> pure $ DEC C
        0x0e -> LD_u8 C <$> fetchByteM
        0x11 -> LD_u16 DE <$> fetchU16M
        0x13 -> pure $ INC16 DE
        0x14 -> pure $ INC D
        0x15 -> pure $ DEC D
        0x16 -> LD_u8 D <$> fetchByteM
        0x17 -> pure RLA
        0x1a -> pure LD_A_derefDE
        0x1b -> pure $ DEC16 DE
        0x1c -> pure $ INC E
        0x1d -> pure $ DEC E
        0x1e -> LD_u8 E <$> fetchByteM
        0x20 -> JR_NZ_i8 <$> fetchI8M
        0x21 -> LD_u16 HL <$> fetchU16M
        0x22 -> pure LD_HLplus_A
        0x23 -> pure $ INC16 HL
        0x24 -> pure $ INC H
        0x25 -> pure $ DEC H
        0x26 -> LD_u8 H <$> fetchByteM
        0x2b -> pure $ DEC16 HL
        0x2c -> pure $ INC L
        0x2d -> pure $ DEC L
        0x2e -> LD_u8 L <$> fetchByteM
        0x31 -> LD_u16 SP <$> fetchU16M
        0x32 -> pure LD_HLminus_A
        0x33 -> pure $ INC16 SP
        0x34 -> pure INC_derefHL
        0x35 -> pure DEC_derefHL
        0x3b -> pure $ DEC16 SP
        0x3c -> pure $ INC A
        0x3d -> pure $ DEC A
        0x3e -> LD_u8 A <$> fetchByteM
        0x40 -> pure $ LD B B
        0x41 -> pure $ LD B C
        0x42 -> pure $ LD B D
        0x43 -> pure $ LD B E
        0x44 -> pure $ LD B H
        0x45 -> pure $ LD B L
        0x47 -> pure $ LD B A
        0x48 -> pure $ LD C B
        0x49 -> pure $ LD C C
        0x4a -> pure $ LD C D
        0x4b -> pure $ LD C E
        0x4c -> pure $ LD C H
        0x4d -> pure $ LD C L
        0x4f -> pure $ LD C A
        0x50 -> pure $ LD D B
        0x51 -> pure $ LD D C
        0x52 -> pure $ LD D D
        0x53 -> pure $ LD D E
        0x54 -> pure $ LD D H
        0x55 -> pure $ LD D L
        0x57 -> pure $ LD D A
        0x58 -> pure $ LD E B
        0x59 -> pure $ LD E C
        0x5a -> pure $ LD E D
        0x5b -> pure $ LD E E
        0x5c -> pure $ LD E H
        0x5d -> pure $ LD E L
        0x5f -> pure $ LD E A
        0x60 -> pure $ LD H B
        0x61 -> pure $ LD H C
        0x62 -> pure $ LD H D
        0x63 -> pure $ LD H E
        0x64 -> pure $ LD H H
        0x65 -> pure $ LD H L
        0x67 -> pure $ LD H A
        0x68 -> pure $ LD L B
        0x69 -> pure $ LD L C
        0x6a -> pure $ LD L D
        0x6b -> pure $ LD L E
        0x6c -> pure $ LD L H
        0x6d -> pure $ LD L L
        0x6f -> pure $ LD L A
        0x77 -> pure LD_derefHL_A
        0x78 -> pure $ LD A B
        0x79 -> pure $ LD A C
        0x7a -> pure $ LD A D
        0x7b -> pure $ LD A E
        0x7c -> pure $ LD A H
        0x7d -> pure $ LD A L
        0x7f -> pure $ LD A A
        0xaf -> pure XOR_A
        0xc1 -> pure POP_BC
        0xc3 -> JP_u16 <$> fetchU16M
        0xc5 -> pure PUSH_BC
        0xc9 -> pure RET
        0xcb -> fetchPrefixed bus
        0xcd -> CALL <$> fetchU16M
        0xe0 -> LD_FF00plusU8_A <$> fetchByteM
        0xe2 -> pure LD_FF00plusC_A
        0xf0 -> LD_A_FF00plusU8 <$> fetchByteM
        0xf3 -> pure DI
        0xfb -> pure EI
        0xfe -> CP_A_u8 <$> fetchByteM
        unknown -> error $ "unknown opcode: " <> toHex unknown

fetchPrefixed :: CPU m => MemoryBus -> m Instr
fetchPrefixed bus = do
    n <- readU16 bus <$> use programCounter
    advance 1
    case n of
        0x11 -> pure RL_C
        0x48 -> pure $ BIT 1 B
        0x49 -> pure $ BIT 1 C
        0x4a -> pure $ BIT 1 D
        0x4b -> pure $ BIT 1 E
        0x4c -> pure $ BIT 1 H
        0x4d -> pure $ BIT 1 L
        0x4e -> pure $ BIT_n_derefHL 1
        0x4f -> pure $ BIT 1 A
        0x50 -> pure $ BIT 2 B
        0x51 -> pure $ BIT 2 C
        0x52 -> pure $ BIT 2 D
        0x53 -> pure $ BIT 2 E
        0x54 -> pure $ BIT 2 H
        0x55 -> pure $ BIT 2 L
        0x56 -> pure $ BIT_n_derefHL 2
        0x57 -> pure $ BIT 2 A
        0x58 -> pure $ BIT 3 B
        0x59 -> pure $ BIT 3 C
        0x5a -> pure $ BIT 3 D
        0x5b -> pure $ BIT 3 E
        0x5c -> pure $ BIT 3 H
        0x5d -> pure $ BIT 3 L
        0x5e -> pure $ BIT_n_derefHL 3
        0x5f -> pure $ BIT 3 A
        0x60 -> pure $ BIT 4 B
        0x61 -> pure $ BIT 4 C
        0x62 -> pure $ BIT 4 D
        0x63 -> pure $ BIT 4 E
        0x64 -> pure $ BIT 4 H
        0x65 -> pure $ BIT 4 L
        0x66 -> pure $ BIT_n_derefHL 4
        0x67 -> pure $ BIT 4 A
        0x68 -> pure $ BIT 5 B
        0x69 -> pure $ BIT 5 C
        0x6a -> pure $ BIT 5 D
        0x6b -> pure $ BIT 5 E
        0x6c -> pure $ BIT 5 H
        0x6d -> pure $ BIT 5 L
        0x6e -> pure $ BIT_n_derefHL 5
        0x6f -> pure $ BIT 5 A
        0x70 -> pure $ BIT 6 B
        0x71 -> pure $ BIT 6 C
        0x72 -> pure $ BIT 6 D
        0x73 -> pure $ BIT 6 E
        0x74 -> pure $ BIT 6 H
        0x75 -> pure $ BIT 6 L
        0x76 -> pure $ BIT_n_derefHL 6
        0x77 -> pure $ BIT 6 A
        0x78 -> pure $ BIT 7 B
        0x79 -> pure $ BIT 7 C
        0x7a -> pure $ BIT 7 D
        0x7b -> pure $ BIT 7 E
        0x7c -> pure $ BIT 7 H
        0x7d -> pure $ BIT 7 L
        0x7e -> pure $ BIT_n_derefHL 7
        0x7f -> pure $ BIT 7 A
        s -> error $ "unknown prefixed byte: " <> toHex s

writeMemory :: CPU m => U16 -> U8 -> m ()
writeMemory addr n =
    case addr of
        -- HACK: "listen" for changes that potentially cascade to other state
        -- changes here
        0xff07 -> do
            freq <- use (memoryBus % timerFrequency)
            modifying' memoryBus (writeByte addr n)
            freq' <- use (memoryBus % timerFrequency)
            when (freq' /= freq) $
                assign' timerCounter (counterFromFrequency freq')
        _ -> modifying' memoryBus (writeByte addr n)

push :: CPU m => U16 -> m ()
push n = do
    s <- get
    let curr = s ^. stackPointer
    modifying' stackPointer (\x -> x - 2)
    writeMemory (curr - 1) hi
    writeMemory (curr - 2) lo
  where
    (hi, lo) = splitIntoBytes n

pop :: CPU m => m U16
pop = do
    -- TODO: do I have to zero the popped memory location?
    s <- get
    put (s & registers % sp %~ (+ 2))
    pure $ readU16 (view memoryBus s) (view stackPointer s)

dec :: CPU m => Lens' Registers U8 -> m Int
dec reg = do
    modify' $ \s ->
        let
            old = s ^. registers % reg
            result = old - 1
        in
            s
                & registers
                    %~ ( set (flag Zero) (result == 0)
                            . setFlag Negative
                            . set (flag HalfCarry) (old .&. 0x0f == 0)
                            . set reg result
                       )
    pure 4

inc :: CPU m => Lens' Registers U8 -> m Int
inc reg = do
    modify' $ \s ->
        let
            old = s ^. registers % reg
            result = old + 1
        in
            s
                & registers
                    %~ ( set (flag Zero) (result == 0)
                            . clearFlag Negative
                            . set (flag HalfCarry) (old .&. 0x0f == 0x0f)
                            . set reg result
                       )
    pure 4

ld_r_r :: CPU m => TargetRegister -> TargetRegister -> m Int
ld_r_r r r' = do
    modify' $ \s ->
        s & registers % (targetL r) .~ (s ^. registers % targetL r')
    pure 4

execute :: CPU m => Instr -> m Int
execute = \case
    NOP -> pure 4
    XOR_A ->
        assign' (registers % a) 0 >> pure 4
    LD_u16 rr n ->
        assign' (registers % target16L rr) n >> pure 12
    LD_HLminus_A -> do
        modifying' (registers % hl) (\x -> x - 1)
        rs <- use registers
        writeMemory (rs ^. hl) (rs ^. a)
        pure 8
    LD_HLplus_A -> do
        modifying' (registers % hl) (+ 1)
        rs <- use registers
        writeMemory (rs ^. hl) (rs ^. a)
        pure 8
    LD_A_derefDE -> do
        s <- get
        let addr = s ^. registers % de
        assign' (registers % a) (readByte (view memoryBus s) addr)
        pure 8
    LD_A_FF00plusU8 n -> do
        modify' $ \s ->
            s & registers % a .~ readByte (view memoryBus s) (0xff00 + fromIntegral n)
        pure 12
    LD_u8 r n ->
        assign' (registers % targetL r) n >> pure 8
    LD r r' ->
        ld_r_r r r'
    LD_FF00plusC_A -> do
        s <- get
        let offset = fromIntegral $ s ^. registers % c
        writeMemory (0xff00 + offset) (s ^. registers % a)
        pure 8
    LD_FF00plusU8_A n -> do
        s <- get
        writeMemory (0xff00 + fromIntegral n) (s ^. registers % a)
        pure 12
    LD_derefHL_A -> do
        s <- get
        writeMemory (s ^. registers % hl) (s ^. registers % a)
        pure 8
    BIT n r -> do
        modify' $ \s ->
            let bitIsSet = Bits.testBit (s ^. registers % targetL r) n
            in s
                & registers
                    %~ setFlag HalfCarry
                    . clearFlag Negative
                    . set (flag Zero) (not bitIsSet)
        pure 8
    BIT_n_derefHL n -> do
        modify' $ \s ->
            let
                p = s ^. registers % hl
                val = readByte (view memoryBus s) p
                bitIsSet = Bits.testBit val n
            in
                s
                    & registers
                        %~ setFlag HalfCarry
                        . clearFlag Negative
                        . set (flag Zero) (not bitIsSet)
        pure 12
    JR_NZ_i8 n -> do
        modify' $ \s ->
            if not $ hasFlag Zero s
                then s & programCounter %~ (+ fromIntegral n)
                else s
        pure 20
    JP_u16 n ->
        assign' (registers % pc) n >> pure 16
    INC r ->
        inc (targetL r)
    INC_derefHL -> do
        s <- get
        let
            addr = s ^. registers % hl
            val = readByte (view memoryBus s) addr + 1
        writeMemory addr val
        pure 12
    INC16 r ->
        modifying' (registers % (target16L r)) (+ 1) >> pure 8
    DEC r ->
        dec (targetL r)
    DEC_derefHL -> do
        s <- get
        let
            addr = s ^. registers % hl
            val = readByte (view memoryBus s) addr - 1
        writeMemory addr val
        modifying'
            registers
            ( set (flag Zero) (val == 0)
                . setFlag Negative
                . set (flag HalfCarry) ((val + 1) .&. 0x0f == 0)
            )
        pure 12
    DEC16 r ->
        modifying' (registers % (target16L r)) (\n -> n - 1) >> pure 8
    CALL n -> do
        counter <- use programCounter
        push (counter + 1)
        assign' (registers % pc) n
        pure 24
    RET -> do
        addr <- pop
        assign' (registers % pc) addr
        pure 16
    PUSH_BC -> do
        n <- use (registers % bc)
        push n
        pure 16
    POP_BC -> do
        n <- pop
        assign' (registers % bc) n
        pure 12
    RLA -> do
        modify' $ \s ->
            let
                carry = if hasFlag Carry s then 1 else 0
                carry' = Bits.testBit (s ^. registers % a) 7
                a' = Bits.shiftL (s ^. registers % a) 1 + carry
            in
                s
                    & registers
                        %~ set (flag Carry) carry'
                        . clearFlag Zero -- TODO: check this: manual says yes, impls say no
                        . clearFlag HalfCarry
                        . set a a'
        pure 4
    RL_C -> do
        modify' $ \s ->
            let
                carry = if hasFlag Carry s then 1 else 0
                carry' = Bits.testBit (s ^. registers % c) 7
                c' = Bits.shiftL (s ^. registers % c) 1 + carry
            in
                s
                    & registers
                        %~ set (flag Carry) carry'
                        . set (flag Zero) (c' == 0)
                        . clearFlag HalfCarry
                        . set c c'
        pure 4
    DI ->
        assign' masterInterruptEnable False >> pure 4
    EI ->
        assign' masterInterruptEnable True >> pure 4
    CP_A_u8 n -> do
        modify' $ \s ->
            let val = s ^. registers % a
            in s
                & registers
                    %~ set (flag Zero) (val == n)
                    . setFlag Negative
                    -- TODO: implement half carry
                    . set (flag Carry) (val < n)
        pure 8

updateTimers :: CPU m => Int -> m ()
updateTimers cycles = do
    updateDivider cycles
    s <- get
    when (s ^. memoryBus % timerEnable) $ do
        let counter' = view timerCounter s - cycles
        assign' timerCounter counter'
        traceShowM counter'
        when (counter' <= 0) $ do
            freq <- use (memoryBus % timerFrequency)
            assign' timerCounter (counterFromFrequency freq)
            if s ^. memoryBus % tima == maxBound
                then do
                    assign' (memoryBus % tima) (s ^. memoryBus % tma)
                    assign' (memoryBus % timerIntRequested) True
                else modifying' (memoryBus % tima) (+ 1)

updateDivider :: CPU m => Int -> m ()
updateDivider cycles = do
    counter <- use dividerCounter
    let counter' = counter + cycles
    assign' dividerCounter counter'
    when (counter' >= 255) $ do
        assign' dividerCounter 0
        modifying' (memoryBus % divider) (+ 1)

data TimerFrequency
    = Freq4K
    | Freq16K
    | Freq64K
    | Freq256K
    deriving (Eq)

readTimerFrequency :: U8 -> TimerFrequency
readTimerFrequency n =
    case (n ^. bit 1, n ^. bit 0) of
        (False, False) -> Freq4K
        (False, True) -> Freq256K
        (True, False) -> Freq64K
        (True, True) -> Freq16K

counterFromFrequency :: TimerFrequency -> Int
counterFromFrequency = \case
    Freq4K -> 1024
    Freq16K -> 256
    Freq64K -> 64
    Freq256K -> 16

timerFrequency :: Getter MemoryBus TimerFrequency
timerFrequency = tac % to readTimerFrequency

handleInterrupts :: CPU m => m ()
handleInterrupts = do
    s <- get
    when (view masterInterruptEnable s) $ do
        let
            enabledInterrupts = s ^. memoryBus % ie
            requestedInterrupts = s ^. memoryBus % interruptFlags
        when (requestedInterrupts > 0) $
            forM_ [0 .. 4] $ \interrupt ->
                when (requestedInterrupts ^. bit interrupt) $ do
                    when (enabledInterrupts ^. bit interrupt) $ do
                        handleInterrupt interrupt
  where
    handleInterrupt interrupt = do
        assign' masterInterruptEnable False
        assign' (memoryBus % interruptFlags % bit interrupt) False
        counter <- use programCounter
        push counter
        -- TODO: check whether I have to execute 2 NOPs
        case interrupt of
            0 -> assign' programCounter 0x40 -- VBlank
            1 -> assign' programCounter 0x48 -- LCD stat
            2 -> assign' programCounter 0x50 -- Timer
            3 -> assign' programCounter 0x58 -- Serial
            4 -> assign' programCounter 0x60 -- Joypad
            s -> error $ "unhandled interrupt: " <> show s
