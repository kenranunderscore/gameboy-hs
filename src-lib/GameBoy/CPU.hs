{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module GameBoy.CPU where

import Control.Monad
import Control.Monad.State.Strict
import Data.Bits ((.&.), (.|.))
import Data.Bits qualified as Bits
import Debug.Trace
import Optics

import GameBoy.BitStuff
import GameBoy.Memory
import GameBoy.State

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

hasFlag :: Flag -> CPUState -> Bool
hasFlag fl = view (registers % flag fl)

advance :: GameBoy m => U16 -> m ()
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

data FlagCondition = ZUnset | ZSet | CUnset | CSet

instance Show FlagCondition where
    show = \case
        ZUnset -> "NZ"
        ZSet -> "Z"
        CUnset -> "NC"
        CSet -> "C"

checkFlagCondition :: FlagCondition -> CPUState -> Bool
checkFlagCondition cond s = check s
  where
    check = case cond of
        ZSet -> hasFlag Zero
        ZUnset -> not . hasFlag Zero
        CSet -> hasFlag Carry
        CUnset -> not . hasFlag Carry

data RestartAddr
    = Rst00
    | Rst08
    | Rst10
    | Rst18
    | Rst20
    | Rst28
    | Rst30
    | Rst38

getRestartAddr :: RestartAddr -> U16
getRestartAddr = \case
    Rst00 -> 0x00
    Rst08 -> 0x08
    Rst10 -> 0x10
    Rst18 -> 0x18
    Rst20 -> 0x20
    Rst28 -> 0x28
    Rst30 -> 0x30
    Rst38 -> 0x38

instance Show RestartAddr where
    show = toHex . getRestartAddr

-- NOTE: as of now, things like PUSH SP are possible, which is not a valid
-- instruction (but could nevertheless be used)
data Instr
    = LD_u16 TargetRegister16 U16
    | LD_A_derefDE
    | LD_A_FF00plusU8 U8
    | LD_A_HLplus
    | LD_A_HLminus
    | LD_A_derefU16 U16
    | LD_u8 TargetRegister U8
    | LD_FF00plusC_A
    | LD_FF00plusU8_A U8
    | LD_u16_A U16
    | LD_derefHL_A
    | LD_HLminus_A
    | LD_HLplus_A
    | LD_HLderef_u8 U8
    | LD_r_HLderef TargetRegister
    | LD TargetRegister TargetRegister
    | LD_HL_SP I8
    | BIT Int TargetRegister
    | BIT_n_derefHL Int
    | JP U16
    | JP_cc FlagCondition U16
    | JR_cc FlagCondition I8
    | JR I8
    | INC TargetRegister
    | INC_derefHL
    | INC16 TargetRegister16
    | DEC TargetRegister
    | DEC_derefHL
    | DEC16 TargetRegister16
    | CALL U16
    | CALL_cc FlagCondition U16
    | RET
    | RET_cc FlagCondition
    | RETI
    | PUSH TargetRegister16
    | PUSH_AF
    | POP TargetRegister16
    | POP_AF
    | RLA
    | RL_C
    | DI
    | EI
    | NOP
    | OR TargetRegister
    | OR_u8 U8
    | AND TargetRegister
    | AND_u8 U8
    | SUB TargetRegister
    | SUB_u8 U8
    | ADD TargetRegister
    | ADD_u8 U8
    | ADC TargetRegister
    | ADC_u8 U8
    | SBC TargetRegister
    | SBC_u8 U8
    | CP TargetRegister
    | CP_u8 U8
    | XOR TargetRegister
    | XOR_u8 U8
    | RST RestartAddr
    | CPL
    | SWAP TargetRegister
    | SWAP_derefHL

instance Show Instr where
    show = \case
        LD_u16 rr n -> "LD " <> show rr <> "," <> toHex n
        LD_derefHL_A -> "LD (HL),A"
        LD_r_HLderef r -> "LD " <> show r <> ",(HL)"
        LD_HLminus_A -> "LD (HL-),A"
        LD_HLderef_u8 n -> "LD (HL)," <> toHex n
        LD_HLplus_A -> "LD (HL+),A"
        LD_A_HLplus -> "LD A,(HL+)"
        LD_A_HLminus -> "LD A,(HL-)"
        LD_A_derefDE -> "LD A,(DE)"
        LD_A_FF00plusU8 n -> "LD A,($ff00+" <> toHex n <> ")"
        LD_A_derefU16 n -> "LD A,(" <> toHex n <> ")"
        LD r r' -> "LD " <> show r <> "," <> show r'
        LD_u8 r n -> "LD " <> show r <> "," <> toHex n
        LD_FF00plusC_A -> "LD ($ff00+C,A)"
        LD_FF00plusU8_A n -> "LD ($ff00+" <> toHex n <> "),A"
        LD_u16_A n -> "LD (" <> toHex n <> "),A"
        LD_HL_SP n -> "LD HL,SP" <> (if n < 0 then mempty else "+") <> toHex n
        BIT n r -> "BIT " <> show n <> "," <> show r
        BIT_n_derefHL n -> "BIT " <> show n <> ",(HL)"
        JP n -> "JP " <> toHex n
        JP_cc cond n -> "JP " <> show cond <> "," <> toHex n
        JR_cc cond n -> "JR " <> show cond <> "," <> show n
        JR n -> "JR " <> show n
        INC r -> "INC " <> show r
        INC_derefHL -> "INC (HL)"
        INC16 r -> "INC " <> show r
        DEC r -> "DEC " <> show r
        DEC_derefHL -> "DEC (HL)"
        DEC16 r -> "DEC " <> show r
        CALL n -> "CALL " <> toHex n
        CALL_cc cond n -> "CALL " <> show cond <> "," <> toHex n
        RET -> "RET"
        RET_cc cond -> "RET " <> show cond
        RETI -> "RETI"
        PUSH rr -> "PUSH " <> show rr
        PUSH_AF -> "PUSH AF"
        POP rr -> "POP " <> show rr
        POP_AF -> "POP AF"
        RLA -> "RLA"
        RL_C -> "RL C"
        DI -> "DI"
        EI -> "EI"
        NOP -> "NOP"
        OR r -> "OR A," <> show r
        OR_u8 n -> "OR A," <> toHex n
        AND r -> "AND A," <> show r
        AND_u8 n -> "AND A," <> toHex n
        SUB r -> "SUB A," <> show r
        SUB_u8 n -> "SUB A," <> toHex n
        ADD r -> "ADD A," <> show r
        ADD_u8 n -> "ADD A," <> toHex n
        ADC r -> "ADC A," <> show r
        ADC_u8 n -> "ADC A," <> toHex n
        SBC r -> " A," <> show r
        SBC_u8 n -> "SBC A," <> toHex n
        XOR r -> "XOR A," <> show r
        XOR_u8 n -> "XOR A," <> toHex n
        CP r -> "CP A," <> show r
        CP_u8 n -> "CP A," <> toHex n
        RST addr -> "RST " <> show addr
        CPL -> "CPL"
        SWAP r -> "SWAP " <> show r
        SWAP_derefHL -> "SWAP (HL)"

fetchByteM :: GameBoy m => m U8
fetchByteM = do
    s <- get
    advance 1
    pure $ readByte (view memoryBus s) (view programCounter s)

fetchI8M :: GameBoy m => m I8
fetchI8M = do
    fromIntegral <$> fetchByteM

fetchU16M :: GameBoy m => m U16
fetchU16M = do
    s <- get
    advance 2
    pure $ readU16 (view memoryBus s) (view programCounter s)

fetch :: GameBoy m => m Instr
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
        0x20 -> JR_cc ZUnset <$> fetchI8M
        0x21 -> LD_u16 HL <$> fetchU16M
        0x22 -> pure LD_HLplus_A
        0x23 -> pure $ INC16 HL
        0x24 -> pure $ INC H
        0x25 -> pure $ DEC H
        0x26 -> LD_u8 H <$> fetchByteM
        0x28 -> JR_cc ZSet <$> fetchI8M
        0x2a -> pure LD_A_HLplus
        0x2b -> pure $ DEC16 HL
        0x2c -> pure $ INC L
        0x2d -> pure $ DEC L
        0x2e -> LD_u8 L <$> fetchByteM
        0x2f -> pure CPL
        0x30 -> JR_cc CUnset <$> fetchI8M
        0x31 -> LD_u16 SP <$> fetchU16M
        0x32 -> pure LD_HLminus_A
        0x33 -> pure $ INC16 SP
        0x34 -> pure INC_derefHL
        0x35 -> pure DEC_derefHL
        0x36 -> LD_HLderef_u8 <$> fetchByteM
        0x38 -> JR_cc CSet <$> fetchI8M
        0x3a -> pure LD_A_HLminus
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
        0x46 -> pure $ LD_r_HLderef B
        0x47 -> pure $ LD B A
        0x48 -> pure $ LD C B
        0x49 -> pure $ LD C C
        0x4a -> pure $ LD C D
        0x4b -> pure $ LD C E
        0x4c -> pure $ LD C H
        0x4d -> pure $ LD C L
        0x4e -> pure $ LD_r_HLderef C
        0x4f -> pure $ LD C A
        0x50 -> pure $ LD D B
        0x51 -> pure $ LD D C
        0x52 -> pure $ LD D D
        0x53 -> pure $ LD D E
        0x54 -> pure $ LD D H
        0x55 -> pure $ LD D L
        0x56 -> pure $ LD_r_HLderef D
        0x57 -> pure $ LD D A
        0x58 -> pure $ LD E B
        0x59 -> pure $ LD E C
        0x5a -> pure $ LD E D
        0x5b -> pure $ LD E E
        0x5c -> pure $ LD E H
        0x5d -> pure $ LD E L
        0x5e -> pure $ LD_r_HLderef E
        0x5f -> pure $ LD E A
        0x60 -> pure $ LD H B
        0x61 -> pure $ LD H C
        0x62 -> pure $ LD H D
        0x63 -> pure $ LD H E
        0x64 -> pure $ LD H H
        0x65 -> pure $ LD H L
        0x66 -> pure $ LD_r_HLderef H
        0x67 -> pure $ LD H A
        0x68 -> pure $ LD L B
        0x69 -> pure $ LD L C
        0x6a -> pure $ LD L D
        0x6b -> pure $ LD L E
        0x6c -> pure $ LD L H
        0x6d -> pure $ LD L L
        0x6e -> pure $ LD_r_HLderef L
        0x6f -> pure $ LD L A
        0x77 -> pure LD_derefHL_A
        0x78 -> pure $ LD A B
        0x79 -> pure $ LD A C
        0x7a -> pure $ LD A D
        0x7b -> pure $ LD A E
        0x7c -> pure $ LD A H
        0x7d -> pure $ LD A L
        0x7e -> pure $ LD_r_HLderef A
        0x7f -> pure $ LD A A
        0x80 -> pure $ ADD B
        0x81 -> pure $ ADD C
        0x82 -> pure $ ADD D
        0x83 -> pure $ ADD E
        0x84 -> pure $ ADD H
        0x85 -> pure $ ADD L
        0x87 -> pure $ ADD A
        0x88 -> pure $ ADC B
        0x89 -> pure $ ADC C
        0x8a -> pure $ ADC D
        0x8b -> pure $ ADC E
        0x8c -> pure $ ADC H
        0x8d -> pure $ ADC L
        0x8f -> pure $ ADC A
        0x90 -> pure $ SUB B
        0x91 -> pure $ SUB C
        0x92 -> pure $ SUB D
        0x93 -> pure $ SUB E
        0x94 -> pure $ SUB H
        0x95 -> pure $ SUB L
        0x97 -> pure $ SUB A
        0x98 -> pure $ SBC B
        0x99 -> pure $ SBC C
        0x9a -> pure $ SBC D
        0x9b -> pure $ SBC E
        0x9c -> pure $ SBC H
        0x9d -> pure $ SBC L
        0x9f -> pure $ SBC A
        0xa0 -> pure $ AND B
        0xa1 -> pure $ AND C
        0xa2 -> pure $ AND D
        0xa3 -> pure $ AND E
        0xa4 -> pure $ AND H
        0xa5 -> pure $ AND L
        0xa7 -> pure $ AND A
        0xa8 -> pure $ XOR B
        0xa9 -> pure $ XOR C
        0xaa -> pure $ XOR D
        0xab -> pure $ XOR E
        0xac -> pure $ XOR H
        0xad -> pure $ XOR L
        0xaf -> pure $ XOR A
        0xb0 -> pure $ OR B
        0xb1 -> pure $ OR C
        0xb2 -> pure $ OR D
        0xb3 -> pure $ OR E
        0xb4 -> pure $ OR H
        0xb5 -> pure $ OR L
        0xb7 -> pure $ OR A
        0xb8 -> pure $ CP B
        0xb9 -> pure $ CP C
        0xba -> pure $ CP D
        0xbb -> pure $ CP E
        0xbc -> pure $ CP H
        0xbd -> pure $ CP L
        0xbf -> pure $ CP A
        0xc0 -> pure $ RET_cc ZUnset
        0xc1 -> pure $ POP BC
        0xc2 -> JP_cc ZUnset <$> fetchU16M
        0xc3 -> JP <$> fetchU16M
        0xc4 -> CALL_cc ZUnset <$> fetchU16M
        0xc5 -> pure $ PUSH BC
        0xc6 -> ADD_u8 <$> fetchByteM
        0xc7 -> pure $ RST Rst00
        0xc8 -> pure $ RET_cc ZSet
        0xc9 -> pure RET
        0xca -> JP_cc ZSet <$> fetchU16M
        0xcb -> fetchPrefixed
        0xcc -> CALL_cc ZSet <$> fetchU16M
        0xcd -> CALL <$> fetchU16M
        0xce -> ADC_u8 <$> fetchByteM
        0xcf -> pure $ RST Rst08
        0xd0 -> pure $ RET_cc CUnset
        0xd1 -> pure $ POP DE
        0xd2 -> JP_cc CUnset <$> fetchU16M
        0xd4 -> CALL_cc CUnset <$> fetchU16M
        0xd5 -> pure $ PUSH DE
        0xd6 -> SUB_u8 <$> fetchByteM
        0xd7 -> pure $ RST Rst10
        0xd8 -> pure $ RET_cc CSet
        0xd9 -> pure RETI
        0xda -> JP_cc CSet <$> fetchU16M
        0xdc -> CALL_cc CSet <$> fetchU16M
        0xde -> SBC_u8 <$> fetchByteM
        0xdf -> pure $ RST Rst18
        0xe0 -> LD_FF00plusU8_A <$> fetchByteM
        0xe2 -> pure LD_FF00plusC_A
        0xe1 -> pure $ POP HL
        0xe5 -> pure $ PUSH HL
        0xe6 -> AND_u8 <$> fetchByteM
        0xe7 -> pure $ RST Rst20
        0xea -> LD_u16_A <$> fetchU16M
        0xee -> XOR_u8 <$> fetchByteM
        0xef -> pure $ RST Rst28
        0xf0 -> LD_A_FF00plusU8 <$> fetchByteM
        0xf1 -> pure POP_AF
        0xf3 -> pure DI
        0xf5 -> pure PUSH_AF
        0xf6 -> OR_u8 <$> fetchByteM
        0xf7 -> pure $ RST Rst30
        0xf8 -> LD_HL_SP <$> fetchI8M
        0xfa -> LD_A_derefU16 <$> fetchU16M
        0xfb -> pure EI
        0xfe -> CP_u8 <$> fetchByteM
        0xff -> pure $ RST Rst38
        unknown -> error $ "unknown opcode: " <> toHex unknown

fetchPrefixed :: GameBoy m => m Instr
fetchPrefixed = do
    s <- get
    let
        counter = view programCounter s
        bus = view memoryBus s
    advance 1
    case readByte bus counter of
        0x11 -> pure RL_C
        0x30 -> pure $ SWAP B
        0x31 -> pure $ SWAP C
        0x32 -> pure $ SWAP D
        0x33 -> pure $ SWAP E
        0x34 -> pure $ SWAP H
        0x35 -> pure $ SWAP L
        0x36 -> pure SWAP_derefHL
        0x37 -> pure $ SWAP A
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
        n -> error $ "unknown prefixed byte: " <> toHex n

writeMemory :: GameBoy m => U16 -> U8 -> m ()
writeMemory addr n =
    case addr of
        0xff46 ->
            trace "    [DMA TRANSFER]" dmaTransfer n
        -- HACK: "listen" for changes that potentially cascade to other state
        -- changes here
        0xff07 -> do
            freq <- use (memoryBus % timerFrequency)
            modifying' memoryBus (writeByte addr n)
            freq' <- use (memoryBus % timerFrequency)
            when (freq' /= freq) $
                assign' timerCounter (counterFromFrequency freq')
        _ -> modifying' memoryBus (writeByte addr n)

push :: GameBoy m => U16 -> m ()
push n = do
    s <- get
    let curr = s ^. stackPointer
    modifying' stackPointer (\x -> x - 2)
    writeMemory (curr - 1) hi
    writeMemory (curr - 2) lo
  where
    (hi, lo) = splitIntoBytes n

pop :: GameBoy m => m U16
pop = do
    -- TODO: do I have to zero the popped memory location?
    s <- get
    put (s & registers % sp %~ (+ 2))
    pure $ readU16 (view memoryBus s) (view stackPointer s)

dec :: GameBoy m => Lens' Registers U8 -> m Int
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

inc :: GameBoy m => Lens' Registers U8 -> m Int
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

ld_r_r :: GameBoy m => TargetRegister -> TargetRegister -> m Int
ld_r_r r r' = do
    modify' $ \s ->
        s & registers % (targetL r) .~ (s ^. registers % targetL r')
    pure 4

-- TODOs:
-- - understand instructions taking longer depending on branching
execute :: GameBoy m => Instr -> m Int
execute = \case
    NOP -> pure 4
    LD_u16 rr n ->
        assign' (registers % target16L rr) n >> pure 12
    LD_r_HLderef r -> do
        s <- get
        let n = readByte (view memoryBus s) (s ^. registers % hl)
        assign' (registers % targetL r) n
        pure 8
    LD_HLminus_A -> do
        rs <- use registers
        writeMemory (rs ^. hl) (rs ^. a)
        modifying' (registers % hl) (\x -> x - 1)
        pure 8
    LD_HLplus_A -> do
        rs <- use registers
        writeMemory (rs ^. hl) (rs ^. a)
        modifying' (registers % hl) (+ 1)
        pure 8
    LD_HLderef_u8 n -> do
        rs <- use registers
        writeMemory (rs ^. hl) n
        pure 12
    LD_A_derefDE -> do
        s <- get
        let addr = s ^. registers % de
        assign' (registers % a) (readByte (view memoryBus s) addr)
        pure 8
    LD_A_HLplus -> do
        s <- get
        modifying'
            registers
            ( \rs ->
                rs
                    & a .~ readByte (view memoryBus s) (rs ^. hl)
                    & hl %~ (+ 1)
            )
        pure 8
    LD_A_HLminus -> do
        s <- get
        modifying'
            registers
            ( \rs ->
                rs
                    & a .~ readByte (view memoryBus s) (rs ^. hl)
                    & hl %~ (\x -> x - 1)
            )
        pure 8
    LD_A_FF00plusU8 n -> do
        modify' $ \s ->
            s & registers % a .~ readByte (view memoryBus s) (0xff00 + fromIntegral n)
        pure 12
    LD_A_derefU16 n -> do
        modify' $ \s ->
            s & registers % a .~ readByte (view memoryBus s) n
        pure 16
    LD_u8 r n ->
        assign' (registers % targetL r) n >> pure 8
    LD r r' ->
        ld_r_r r r'
    LD_u16_A n -> do
        rs <- use registers
        writeMemory n (rs ^. a)
        pure 16
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
    LD_HL_SP n -> do
        modifying' registers $ \rs ->
            let
                -- FIXME: this is most definitely wrong!!
                orig = rs ^. sp
                n' = fromIntegral @_ @U8 n
                res' = fromIntegral @_ @U16 orig + fromIntegral n'
                res = fromIntegral res'
                needsHalfCarry = n' .&. 0x0f + fromIntegral orig .&. 0x0f > 0x0f
            in
                rs
                    & hl .~ res
                    & clearFlag Zero
                    & clearFlag Negative
                    & set (flag Carry) (res' > 0xff)
                    & set (flag HalfCarry) needsHalfCarry
        pure 12
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
    JR n -> do
        modifying' programCounter (+ fromIntegral n)
        pure 12
    JR_cc cond n -> do
        modify' $ \s ->
            if checkFlagCondition cond s
                then s & programCounter %~ (+ fromIntegral n)
                else s
        pure 12
    JP_cc cond n -> do
        modify' $ \s ->
            if checkFlagCondition cond s
                then s & programCounter .~ n
                else s
        pure 12
    RET -> do
        addr <- pop
        assign' programCounter addr
        pure 16
    RET_cc cond -> do
        s <- get
        when (checkFlagCondition cond s) $ do
            addr <- pop
            assign' programCounter addr
        pure 16
    RETI -> do
        addr <- pop
        assign' programCounter addr
        assign' masterInterruptEnable True
        pure 16
    CALL n -> do
        counter <- use programCounter
        push counter
        assign' programCounter n
        pure 24
    CALL_cc cond n -> do
        s <- get
        when (checkFlagCondition cond s) $ do
            let counter = view programCounter s
            push counter
            assign' programCounter n
        pure 24
    JP n ->
        assign' programCounter n >> pure 16
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
    PUSH rr -> do
        n <- use (registers % target16L rr)
        push n
        pure 16
    PUSH_AF -> do
        n <- use (registers % af)
        push n
        pure 16
    POP rr -> do
        n <- pop
        assign' (registers % target16L rr) n
        pure 12
    POP_AF -> do
        n <- pop
        assign' (registers % af) n
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
    -- TODO: refactor these with the _u8 variants into using the same
    -- implementations
    OR r -> or_a r
    OR_u8 n -> do
        modifying' registers $ \rs ->
            let res = rs ^. a .|. n
            in rs
                & set a res
                    . clearFlag Negative
                    . clearFlag Carry
                    . clearFlag HalfCarry
                    . set (flag Zero) (res == 0)
        pure 8
    AND r -> and_a r
    AND_u8 n -> do
        modifying' registers $ \rs ->
            let res = rs ^. a .&. n
            in rs
                & set a res
                    . clearFlag Negative
                    . clearFlag Carry
                    . setFlag HalfCarry
                    . set (flag Zero) (res == 0)
        pure 8
    ADD r -> add_a r
    ADD_u8 n -> do
        modifying' registers $ \rs ->
            let
                orig = rs ^. a
                res' = fromIntegral @_ @U16 orig + fromIntegral n
                res = fromIntegral res'
                needsHalfCarry = (orig .&. 0x0f) + (n .&. 0x0f) > 0x0f
                needsCarry = res' > 0xff
            in
                rs
                    & set a res
                        . clearFlag Negative
                        . set (flag Carry) needsCarry
                        . set (flag HalfCarry) needsHalfCarry
                        . set (flag Zero) (res == 0)
        pure 8
    SUB r -> sub_a r
    SUB_u8 n -> do
        modifying' registers $ \rs ->
            let
                orig = rs ^. a
                res = rs ^. a - n
                needsHalfCarry = orig .&. 0x0f < n .&. 0x0f
            in
                rs
                    & set a res
                        . clearFlag Negative
                        . set (flag Carry) (orig < n)
                        . set (flag HalfCarry) needsHalfCarry
                        . set (flag Zero) (res == 0)
        pure 8
    ADC r -> adc_a r
    ADC_u8 n -> do
        modifying' registers $ \rs ->
            let
                orig = rs ^. a
                carry = if rs ^. flag Carry then 1 else 0
                res' = fromIntegral @_ @U16 orig + fromIntegral n + fromIntegral carry
                needsCarry = res' > 0xff
                res = fromIntegral res'
                needsHalfCarry = (orig .&. 0x0f) + (n .&. 0x0f) + carry > 0x0f
            in
                rs
                    & set a res
                        . clearFlag Negative
                        . set (flag Carry) needsCarry
                        . set (flag HalfCarry) needsHalfCarry
                        . set (flag Zero) (res == 0)
        pure 8
    SBC r -> sbc_a r
    SBC_u8 n -> do
        modifying' registers $ \rs ->
            let
                orig = rs ^. a
                carry = if rs ^. flag Carry then 1 else 0
                -- FIXME: check if this correct in case the carry bit overflows
                -- val
                val = n + carry
                res = rs ^. a - val
                needsHalfCarry = orig .&. 0x0f < val .&. 0x0f
            in
                rs
                    & set a res
                        . clearFlag Negative
                        . set (flag Carry) (orig < val)
                        . set (flag HalfCarry) needsHalfCarry
                        . set (flag Zero) (res == 0)
        pure 8
    CP r -> cp_a r
    CP_u8 n -> do
        modify' $ \s ->
            let
                orig = s ^. registers % a
                res = orig - n
                needsHalfCarry = orig .&. 0x0f < n .&. 0x0f
            in
                s
                    & registers
                        %~ set (flag Zero) (res == 0)
                        . setFlag Negative
                        . set (flag HalfCarry) needsHalfCarry
                        . set (flag Carry) (orig < n)
        pure 8
    XOR r -> xor_a r
    XOR_u8 n -> do
        modifying'
            registers
            ( \rs ->
                let res = view a rs `Bits.xor` n
                in rs
                    & flag Zero .~ (res == 0)
                    & clearFlag Negative
                    & clearFlag HalfCarry
                    & clearFlag Carry
                    & a .~ res
            )
        pure 8
    RST addr -> do
        counter <- use programCounter
        push counter
        assign' programCounter (getRestartAddr addr)
        pure 16
    CPL -> do
        modifying' registers $ \rs ->
            rs
                & a %~ Bits.complement
                & setFlag Negative
                & setFlag HalfCarry
        pure 4
    SWAP r -> do
        modifying' registers $ \rs ->
            let orig = view (targetL r) rs
            in rs
                & targetL r %~ (`Bits.rotate` 4)
                & clearFlag Negative
                & clearFlag HalfCarry
                & clearFlag Carry
                & set (flag Zero) (orig == 0)
        pure 8
    SWAP_derefHL -> do
        s <- get
        let
            rs = s ^. registers
            addr = rs ^. hl
            orig = readByte (view memoryBus s) addr
        writeMemory addr (Bits.rotate orig 4)
        modifying'
            registers
            ( clearFlag Negative
                . clearFlag HalfCarry
                . clearFlag Carry
                . set (flag Zero) (orig == 0)
            )
        pure 16

or_a :: GameBoy m => TargetRegister -> m Int
or_a r = do
    modifying' registers $ \rs ->
        let
            val = rs ^. targetL r
            res = rs ^. a .|. val
        in
            rs
                & set a res
                    . clearFlag Negative
                    . clearFlag Carry
                    . clearFlag HalfCarry
                    . set (flag Zero) (res == 0)
    pure 4

and_a :: GameBoy m => TargetRegister -> m Int
and_a r = do
    modifying' registers $ \rs ->
        let
            val = rs ^. targetL r
            res = rs ^. a .&. val
        in
            rs
                & set a res
                    . clearFlag Negative
                    . clearFlag Carry
                    . setFlag HalfCarry
                    . set (flag Zero) (res == 0)
    pure 4

add_a :: GameBoy m => TargetRegister -> m Int
add_a r = do
    modifying' registers $ \rs ->
        let
            orig = rs ^. a
            val = rs ^. targetL r
            res' = fromIntegral @_ @U16 orig + fromIntegral val
            res = fromIntegral res'
            needsHalfCarry = (orig .&. 0x0f) + (val .&. 0x0f) > 0x0f
            needsCarry = res' > 0xff
        in
            rs
                & set a res
                    . clearFlag Negative
                    . set (flag Carry) needsCarry
                    . set (flag HalfCarry) needsHalfCarry
                    . set (flag Zero) (res == 0)
    pure 4

xor_a :: GameBoy m => TargetRegister -> m Int
xor_a r = do
    modifying' registers $ \rs ->
        let
            val = rs ^. targetL r
            res = rs ^. a `Bits.xor` val
        in
            rs
                & set a res
                    . clearFlag Negative
                    . clearFlag Carry
                    . clearFlag HalfCarry
                    . set (flag Zero) (res == 0)
    pure 4

cp_a :: GameBoy m => TargetRegister -> m Int
cp_a r = do
    modifying' registers $ \rs ->
        let
            orig = rs ^. a
            val = rs ^. targetL r
            needsHalfCarry = orig .&. 0x0f < val .&. 0x0f
            res = orig - val
        in
            rs
                & setFlag Negative
                    . set (flag Carry) (orig < val)
                    . set (flag HalfCarry) needsHalfCarry
                    . set (flag Zero) (res == 0)
    pure 4

sbc_a :: GameBoy m => TargetRegister -> m Int
sbc_a r = do
    modifying' registers $ \rs ->
        let
            orig = rs ^. a
            carry = if rs ^. flag Carry then 1 else 0
            -- FIXME: check if this correct in case the carry bit overflows
            -- val
            val = rs ^. targetL r + carry
            res = rs ^. a - val
            needsHalfCarry = orig .&. 0x0f < val .&. 0x0f
        in
            rs
                & set a res
                    . clearFlag Negative
                    . set (flag Carry) (orig < val)
                    . set (flag HalfCarry) needsHalfCarry
                    . set (flag Zero) (res == 0)
    pure 4

sub_a :: GameBoy m => TargetRegister -> m Int
sub_a r = do
    modifying' registers $ \rs ->
        let
            orig = rs ^. a
            val = rs ^. targetL r
            res = rs ^. a - val
            needsHalfCarry = orig .&. 0x0f < val .&. 0x0f
        in
            rs
                & set a res
                    . clearFlag Negative
                    . set (flag Carry) (orig < val)
                    . set (flag HalfCarry) needsHalfCarry
                    . set (flag Zero) (res == 0)
    pure 4

adc_a :: GameBoy m => TargetRegister -> m Int
adc_a r = do
    modifying' registers $ \rs ->
        let
            orig = rs ^. a
            val = rs ^. targetL r
            carry = if rs ^. flag Carry then 1 else 0
            res' = fromIntegral @_ @U16 orig + fromIntegral val + fromIntegral carry
            needsCarry = res' > 0xff
            res = fromIntegral res'
            needsHalfCarry = (orig .&. 0x0f) + (val .&. 0x0f) + carry > 0x0f
        in
            rs
                & set a res
                    . clearFlag Negative
                    . set (flag Carry) needsCarry
                    . set (flag HalfCarry) needsHalfCarry
                    . set (flag Zero) (res == 0)
    pure 4

updateTimers :: GameBoy m => Int -> m ()
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

updateDivider :: GameBoy m => Int -> m ()
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

handleInterrupts :: GameBoy m => m ()
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

dmaTransfer :: GameBoy m => U8 -> m ()
dmaTransfer n = do
    let startAddr :: U16 = Bits.shiftL (fromIntegral n) 8 -- times 0x100
    bus <- use memoryBus
    -- FIXME: use a slice pointing to cartridge memory instead?
    forM_ [0 .. 0xa0 - 1] $ \i ->
        assign' (memoryBus % oam % byte i) (readByte bus (startAddr + i))
