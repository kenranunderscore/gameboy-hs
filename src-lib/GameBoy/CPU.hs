{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module GameBoy.CPU where

import Control.Monad
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict
import Data.Bits ((.&.), (.>>.), (.|.))
import Data.Bits qualified as Bits
import Data.Function ((&))
import Data.Vector qualified as Vector

import GameBoy.BitStuff
import GameBoy.Memory
import GameBoy.State

-- | Flags, living in the higher nibble of the F register.
data Flag = Zero | Negative | HalfCarry | Carry
    deriving (Show)

flagBit :: Flag -> Int
flagBit = \case
    Zero -> 7
    Negative -> 6
    HalfCarry -> 5
    Carry -> 4

adjustFlag :: Flag -> Bool -> Registers -> Registers
adjustFlag fl enable rs =
    let change = if enable then Bits.setBit else Bits.clearBit
    in rs{f = change rs.f (flagBit fl)}

clearFlag :: Flag -> Registers -> Registers
clearFlag fl = adjustFlag fl False

setFlag :: Flag -> Registers -> Registers
setFlag fl = adjustFlag fl True

hasFlag :: Flag -> Registers -> Bool
hasFlag fl rs = Bits.testBit rs.f (flagBit fl)

hasFlag' :: Flag -> CPUState -> Bool
hasFlag' fl s = hasFlag fl s.registers

programCounterM :: GameBoy U16
programCounterM = gets (.registers.pc)

modifyProgramCounter :: (U16 -> U16) -> Registers -> Registers
modifyProgramCounter f rs = rs{pc = f rs.pc}

modifyProgramCounterM :: (U16 -> U16) -> GameBoy ()
modifyProgramCounterM f = modify' $ \s ->
    s{registers = modifyProgramCounter f s.registers}

jumpM :: U16 -> GameBoy ()
jumpM = modifyProgramCounterM . const

modifyStackPointer :: (U16 -> U16) -> Registers -> Registers
modifyStackPointer f rs = rs{sp = f rs.sp}

modifyStackPointerM :: (U16 -> U16) -> GameBoy ()
modifyStackPointerM f = modify' $ \s ->
    s{registers = modifyStackPointer f s.registers}

advance :: U16 -> GameBoy ()
advance n = modifyProgramCounterM (+ n)

data TargetRegister = A | B | C | D | E | H | L
    deriving stock (Show)

readRegister :: TargetRegister -> Registers -> U8
readRegister r rs =
    case r of
        A -> rs.a
        B -> rs.b
        C -> rs.c
        D -> rs.d
        E -> rs.e
        H -> rs.h
        L -> rs.l

modifyRegister :: TargetRegister -> (U8 -> U8) -> Registers -> Registers
modifyRegister r f rs =
    let val = f $ readRegister r rs
    in case r of
        A -> rs{a = val}
        B -> rs{b = val}
        C -> rs{c = val}
        D -> rs{d = val}
        E -> rs{e = val}
        H -> rs{h = val}
        L -> rs{l = val}

modifyRegisterM :: TargetRegister -> (U8 -> U8) -> GameBoy ()
modifyRegisterM r f = modifyRegistersM $ modifyRegister r f

setRegister :: TargetRegister -> U8 -> Registers -> Registers
setRegister r = modifyRegister r . const

setRegister' :: TargetRegister -> U8 -> CPUState -> CPUState
setRegister' r n s = s{registers = setRegister r n s.registers}

setRegisterM :: TargetRegister -> U8 -> GameBoy ()
setRegisterM r n = modifyRegistersM $ setRegister r n

data TargetRegister16 = BC | DE | HL
    deriving stock (Show)

readRegister16 :: TargetRegister16 -> Registers -> U16
readRegister16 rr rs =
    case rr of
        BC -> bc rs
        DE -> de rs
        HL -> hl rs

readRegister16M :: TargetRegister16 -> GameBoy U16
readRegister16M rr = do
    rs <- registersM
    pure $ readRegister16 rr rs

modifyRegister16 :: TargetRegister16 -> (U16 -> U16) -> Registers -> Registers
modifyRegister16 rr f rs =
    let val = f $ readRegister16 rr rs
    in case rr of
        BC -> setBC val rs
        DE -> setDE val rs
        HL -> setHL val rs

modifyRegister16M :: TargetRegister16 -> (U16 -> U16) -> GameBoy ()
modifyRegister16M r f = modifyRegistersM $ modifyRegister16 r f

setRegister16 :: TargetRegister16 -> U16 -> Registers -> Registers
setRegister16 rr = modifyRegister16 rr . const

setRegister16M :: TargetRegister16 -> U16 -> GameBoy ()
setRegister16M rr n = modifyRegistersM $ setRegister16 rr n

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
        ZSet -> hasFlag' Zero
        ZUnset -> not . hasFlag' Zero
        CSet -> hasFlag' Carry
        CUnset -> not . hasFlag' Carry

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

data Instr
    = LD_u16 TargetRegister16 U16
    | LDSP_u16 U16
    | LD_u16SP U16
    | LDSP_HL
    | LD_A_deref TargetRegister16
    | LD_A_FF00plusU8 U8
    | LD_A_FF00plusC
    | LD_A_HLplus
    | LD_A_HLminus
    | LD_A_derefU16 U16
    | LD_u8 TargetRegister U8
    | LD_FF00plusC_A
    | LD_FF00plusU8_A U8
    | LD_u16_A U16
    | LD_deref_rr_A TargetRegister16
    | LD_HLminus_A
    | LD_HLplus_A
    | LD_HLderef_u8 U8
    | LD_r_HLderef TargetRegister
    | LD TargetRegister TargetRegister
    | LD_HLSP I8
    | LD_derefHL TargetRegister
    | BIT Int TargetRegister
    | BIT_n_derefHL Int
    | JP U16
    | JP_HL
    | JP_cc FlagCondition U16
    | JR_cc FlagCondition I8
    | JR I8
    | INC TargetRegister
    | INC_derefHL
    | INC16 TargetRegister16
    | INCSP
    | DEC TargetRegister
    | DEC_derefHL
    | DEC16 TargetRegister16
    | DECSP
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
    | RRA
    | DI
    | EI
    | NOP
    | OR TargetRegister
    | OR_u8 U8
    | OR_A_HL
    | AND TargetRegister
    | AND_u8 U8
    | AND_A_HL
    | SUB TargetRegister
    | SUB_u8 U8
    | SUB_A_HL
    | ADD TargetRegister
    | ADD_u8 U8
    | ADD_A_HL
    | ADC TargetRegister
    | ADC_u8 U8
    | ADC_A_HL
    | SBC TargetRegister
    | SBC_u8 U8
    | SBC_A_HL
    | CP TargetRegister
    | CP_u8 U8
    | CP_A_HL
    | XOR TargetRegister
    | XOR_u8 U8
    | XOR_A_HL
    | RST RestartAddr
    | CPL
    | SWAP TargetRegister
    | SWAP_derefHL
    | ADD_HL TargetRegister16
    | ADD_HLSP
    | ADD_SP I8
    | RES Int TargetRegister
    | RES_derefHL Int
    | SET Int TargetRegister
    | SET_derefHL Int
    | SRL TargetRegister
    | SRL_derefHL
    | RR TargetRegister
    | RR_derefHL
    | RRC TargetRegister
    | RRC_derefHL
    | RL TargetRegister
    | RL_derefHL
    | RLC TargetRegister
    | RLC_derefHL
    | SLA TargetRegister
    | SLA_derefHL
    | SRA TargetRegister
    | SRA_derefHL
    | DAA
    | SCF
    | CCF
    | RLCA
    | RRCA
    | HALT
    | STOP

instance Show Instr where
    show = \case
        LD_u16 rr n -> "LD " <> show rr <> "," <> toHex n
        LDSP_u16 n -> "LD SP," <> toHex n
        LD_u16SP n -> "LD (" <> toHex n <> "),SP"
        LDSP_HL -> "LD SP,HL"
        LD_deref_rr_A rr -> "LD (" <> show rr <> "),A"
        LD_r_HLderef r -> "LD " <> show r <> ",(HL)"
        LD_HLminus_A -> "LD (HL-),A"
        LD_HLderef_u8 n -> "LD (HL)," <> toHex n
        LD_HLplus_A -> "LD (HL+),A"
        LD_A_HLplus -> "LD A,(HL+)"
        LD_A_HLminus -> "LD A,(HL-)"
        LD_A_deref rr -> "LD A,(" <> show rr <> ")"
        LD_A_FF00plusU8 n -> "LD A,($ff00+" <> toHex n <> ")"
        LD_A_FF00plusC -> "LD A,($ff00+C)"
        LD_A_derefU16 n -> "LD A,(" <> toHex n <> ")"
        LD r r' -> "LD " <> show r <> "," <> show r'
        LD_u8 r n -> "LD " <> show r <> "," <> toHex n
        LD_FF00plusC_A -> "LD ($ff00+C,A)"
        LD_FF00plusU8_A n -> "LD ($ff00+" <> toHex n <> "),A"
        LD_u16_A n -> "LD (" <> toHex n <> "),A"
        LD_HLSP n -> "LD HL,SP" <> (if n < 0 then mempty else "+") <> show n
        LD_derefHL r -> "LD (HL)," <> show r
        BIT n r -> "BIT " <> show n <> "," <> show r
        BIT_n_derefHL n -> "BIT " <> show n <> ",(HL)"
        JP n -> "JP " <> toHex n
        JP_HL -> "JP HL"
        JP_cc cond n -> "JP " <> show cond <> "," <> toHex n
        JR_cc cond n -> "JR " <> show cond <> "," <> show n
        JR n -> "JR " <> show n
        INC r -> "INC " <> show r
        INC_derefHL -> "INC (HL)"
        INC16 r -> "INC " <> show r
        INCSP -> "INC SP"
        DEC r -> "DEC " <> show r
        DEC_derefHL -> "DEC (HL)"
        DEC16 r -> "DEC " <> show r
        DECSP -> "DEC SP"
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
        RRA -> "RRA"
        DI -> "DI"
        EI -> "EI"
        NOP -> "NOP"
        OR r -> "OR A," <> show r
        OR_u8 n -> "OR A," <> toHex n
        OR_A_HL -> "OR A,HL"
        AND r -> "AND A," <> show r
        AND_u8 n -> "AND A," <> toHex n
        AND_A_HL -> "AND A,HL"
        SUB r -> "SUB A," <> show r
        SUB_u8 n -> "SUB A," <> toHex n
        SUB_A_HL -> "SUB A,HL"
        ADD r -> "ADD A," <> show r
        ADD_u8 n -> "ADD A," <> toHex n
        ADD_A_HL -> "ADD A,HL"
        ADC r -> "ADC A," <> show r
        ADC_u8 n -> "ADC A," <> toHex n
        ADC_A_HL -> "ADC A,HL"
        SBC r -> " A," <> show r
        SBC_u8 n -> "SBC A," <> toHex n
        SBC_A_HL -> "SBC A,HL"
        XOR r -> "XOR A," <> show r
        XOR_u8 n -> "XOR A," <> toHex n
        XOR_A_HL -> "XOR A,HL"
        CP r -> "CP A," <> show r
        CP_u8 n -> "CP A," <> toHex n
        CP_A_HL -> "CP A,HL"
        RST addr -> "RST " <> show addr
        CPL -> "CPL"
        SWAP r -> "SWAP " <> show r
        SWAP_derefHL -> "SWAP (HL)"
        ADD_HL rr -> "ADD HL," <> show rr
        ADD_HLSP -> "ADD HL,SP"
        ADD_SP n -> "ADD SP," <> show n
        RES n r -> "RES " <> show n <> "," <> show r
        RES_derefHL n -> "RES " <> show n <> ",(HL)"
        SET n r -> "SET " <> show n <> "," <> show r
        SET_derefHL n -> "SET " <> show n <> ",(HL)"
        SRL r -> "SRL " <> show r
        SRL_derefHL -> "SRL (HL)"
        RR r -> "RR " <> show r
        RR_derefHL -> "RR (HL)"
        RRC r -> "RRC " <> show r
        RRC_derefHL -> "RRC (HL)"
        RL r -> "RL " <> show r
        RL_derefHL -> "RL (HL)"
        RLC r -> "RLC " <> show r
        RLC_derefHL -> "RLC (HL)"
        SLA r -> "SLA " <> show r
        SLA_derefHL -> "SLA (HL)"
        SRA r -> "SRA " <> show r
        SRA_derefHL -> "SRA (HL)"
        DAA -> "DAA"
        SCF -> "SCF"
        CCF -> "CCF"
        RLCA -> "RLCA"
        RRCA -> "RRCA"
        HALT -> "HALT"
        STOP -> "STOP"

fetchByteM :: GameBoy U8
fetchByteM = do
    s <- get
    advance 1
    pure $ readByte s.memoryBus s.registers.pc

fetchI8M :: GameBoy I8
fetchI8M = do
    fromIntegral <$> fetchByteM

fetchU16M :: GameBoy U16
fetchU16M = do
    s <- get
    advance 2
    pure $ readU16 s.memoryBus s.registers.pc

{- FOURMOLU_DISABLE -}

lookupCycles :: U8 -> Int
lookupCycles n = 4 * cycles Vector.! fromIntegral n
  where
    cycles = Vector.fromList $!
        [ 1, 3, 2, 2, 1, 1, 2, 1, 5, 2, 2, 2, 1, 1, 2, 1 -- 0
        , 0, 3, 2, 2, 1, 1, 2, 1, 3, 2, 2, 2, 1, 1, 2, 1 -- 1
        , 2, 3, 2, 2, 1, 1, 2, 1, 2, 2, 2, 2, 1, 1, 2, 1 -- 2
        , 2, 3, 2, 2, 3, 3, 3, 1, 2, 2, 2, 2, 1, 1, 2, 1 -- 3
        , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1 -- 4
        , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1 -- 5
        , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1 -- 6
        , 2, 2, 2, 2, 2, 2, 0, 2, 1, 1, 1, 1, 1, 1, 2, 1 -- 7
        , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1 -- 8
        , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1 -- 9
        , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1 -- a
        , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1 -- b
        , 2, 3, 3, 4, 3, 4, 2, 4, 2, 4, 3, 0, 3, 6, 2, 4 -- c
        , 2, 3, 3, 0, 3, 4, 2, 4, 2, 4, 3, 0, 3, 0, 2, 4 -- d
        , 3, 3, 2, 0, 0, 4, 2, 4, 4, 1, 4, 0, 0, 0, 2, 4 -- e
        , 3, 3, 2, 1, 0, 4, 2, 4, 3, 2, 4, 1, 0, 0, 2, 4 -- f
        ]
        -- 0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f

lookupCyclesPrefixed :: U8 -> Int
lookupCyclesPrefixed n = 4 * cycles Vector.! fromIntegral n
  where
    cycles = Vector.fromList $!
        [ 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- 0
        , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- 1
        , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- 2
        , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- 3
        , 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2 -- 4
        , 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2 -- 5
        , 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2 -- 6
        , 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2 -- 7
        , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- 8
        , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- 9
        , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- A
        , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- B
        , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- C
        , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- D
        , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- E
        , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- F
        ]
        -- 0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f

{- FOURMOLU_ENABLE -}

data Instruction = Instruction
    { baseCycles :: Int
    , tag :: Instr
    }

instance Show Instruction where
    show Instruction{tag} = show tag

fetch :: GameBoy Instruction
fetch = do
    s <- get
    let
        counter = s.registers.pc
        bus = s.memoryBus
    advance 1
    let n = readByte bus counter
    if n == 0xcb
        then fetchPrefixed
        else do
            let cycles = lookupCycles n
            instr <- case n of
                0 -> pure NOP
                0x01 -> LD_u16 BC <$> fetchU16M
                0x02 -> pure $ LD_deref_rr_A BC
                0x03 -> pure $ INC16 BC
                0x04 -> pure $ INC B
                0x05 -> pure $ DEC B
                0x06 -> LD_u8 B <$> fetchByteM
                0x07 -> pure RLCA
                0x08 -> LD_u16SP <$> fetchU16M
                0x09 -> pure $ ADD_HL BC
                0x0a -> pure $ LD_A_deref BC
                0x0b -> pure $ DEC16 BC
                0x0c -> pure $ INC C
                0x0d -> pure $ DEC C
                0x0e -> LD_u8 C <$> fetchByteM
                0x0f -> pure RRCA
                0x10 -> advance 1 >> pure STOP
                0x11 -> LD_u16 DE <$> fetchU16M
                0x12 -> pure $ LD_deref_rr_A DE
                0x13 -> pure $ INC16 DE
                0x14 -> pure $ INC D
                0x15 -> pure $ DEC D
                0x16 -> LD_u8 D <$> fetchByteM
                0x17 -> pure RLA
                0x18 -> JR <$> fetchI8M
                0x19 -> pure $ ADD_HL DE
                0x1a -> pure $ LD_A_deref DE
                0x1b -> pure $ DEC16 DE
                0x1c -> pure $ INC E
                0x1d -> pure $ DEC E
                0x1e -> LD_u8 E <$> fetchByteM
                0x1f -> pure RRA
                0x20 -> JR_cc ZUnset <$> fetchI8M
                0x21 -> LD_u16 HL <$> fetchU16M
                0x22 -> pure LD_HLplus_A
                0x23 -> pure $ INC16 HL
                0x24 -> pure $ INC H
                0x25 -> pure $ DEC H
                0x26 -> LD_u8 H <$> fetchByteM
                0x27 -> pure DAA
                0x28 -> JR_cc ZSet <$> fetchI8M
                0x29 -> pure $ ADD_HL HL
                0x2a -> pure LD_A_HLplus
                0x2b -> pure $ DEC16 HL
                0x2c -> pure $ INC L
                0x2d -> pure $ DEC L
                0x2e -> LD_u8 L <$> fetchByteM
                0x2f -> pure CPL
                0x30 -> JR_cc CUnset <$> fetchI8M
                0x31 -> LDSP_u16 <$> fetchU16M
                0x32 -> pure LD_HLminus_A
                0x33 -> pure $ INCSP
                0x34 -> pure INC_derefHL
                0x35 -> pure DEC_derefHL
                0x36 -> LD_HLderef_u8 <$> fetchByteM
                0x37 -> pure SCF
                0x38 -> JR_cc CSet <$> fetchI8M
                0x39 -> pure ADD_HLSP
                0x3a -> pure LD_A_HLminus
                0x3b -> pure DECSP
                0x3c -> pure $ INC A
                0x3d -> pure $ DEC A
                0x3e -> LD_u8 A <$> fetchByteM
                0x3f -> pure CCF
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
                0x70 -> pure $ LD_derefHL B
                0x71 -> pure $ LD_derefHL C
                0x72 -> pure $ LD_derefHL D
                0x73 -> pure $ LD_derefHL E
                0x74 -> pure $ LD_derefHL H
                0x75 -> pure $ LD_derefHL L
                0x76 -> pure HALT
                0x77 -> pure $ LD_derefHL A
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
                0x86 -> pure ADD_A_HL
                0x87 -> pure $ ADD A
                0x88 -> pure $ ADC B
                0x89 -> pure $ ADC C
                0x8a -> pure $ ADC D
                0x8b -> pure $ ADC E
                0x8c -> pure $ ADC H
                0x8d -> pure $ ADC L
                0x8e -> pure ADC_A_HL
                0x8f -> pure $ ADC A
                0x90 -> pure $ SUB B
                0x91 -> pure $ SUB C
                0x92 -> pure $ SUB D
                0x93 -> pure $ SUB E
                0x94 -> pure $ SUB H
                0x95 -> pure $ SUB L
                0x96 -> pure SUB_A_HL
                0x97 -> pure $ SUB A
                0x98 -> pure $ SBC B
                0x99 -> pure $ SBC C
                0x9a -> pure $ SBC D
                0x9b -> pure $ SBC E
                0x9c -> pure $ SBC H
                0x9d -> pure $ SBC L
                0x9e -> pure SBC_A_HL
                0x9f -> pure $ SBC A
                0xa0 -> pure $ AND B
                0xa1 -> pure $ AND C
                0xa2 -> pure $ AND D
                0xa3 -> pure $ AND E
                0xa4 -> pure $ AND H
                0xa5 -> pure $ AND L
                0xa6 -> pure AND_A_HL
                0xa7 -> pure $ AND A
                0xa8 -> pure $ XOR B
                0xa9 -> pure $ XOR C
                0xaa -> pure $ XOR D
                0xab -> pure $ XOR E
                0xac -> pure $ XOR H
                0xad -> pure $ XOR L
                0xae -> pure XOR_A_HL
                0xaf -> pure $ XOR A
                0xb0 -> pure $ OR B
                0xb1 -> pure $ OR C
                0xb2 -> pure $ OR D
                0xb3 -> pure $ OR E
                0xb4 -> pure $ OR H
                0xb5 -> pure $ OR L
                0xb6 -> pure OR_A_HL
                0xb7 -> pure $ OR A
                0xb8 -> pure $ CP B
                0xb9 -> pure $ CP C
                0xba -> pure $ CP D
                0xbb -> pure $ CP E
                0xbc -> pure $ CP H
                0xbd -> pure $ CP L
                0xbe -> pure CP_A_HL
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
                0xe1 -> pure $ POP HL
                0xe2 -> pure LD_FF00plusC_A
                0xe5 -> pure $ PUSH HL
                0xe6 -> AND_u8 <$> fetchByteM
                0xe7 -> pure $ RST Rst20
                0xe8 -> ADD_SP <$> fetchI8M
                0xe9 -> pure JP_HL
                0xea -> LD_u16_A <$> fetchU16M
                0xee -> XOR_u8 <$> fetchByteM
                0xef -> pure $ RST Rst28
                0xf0 -> LD_A_FF00plusU8 <$> fetchByteM
                0xf1 -> pure POP_AF
                0xf2 -> pure LD_A_FF00plusC
                0xf3 -> pure DI
                0xf5 -> pure PUSH_AF
                0xf6 -> OR_u8 <$> fetchByteM
                0xf7 -> pure $ RST Rst30
                0xf8 -> LD_HLSP <$> fetchI8M
                0xf9 -> pure LDSP_HL
                0xfa -> LD_A_derefU16 <$> fetchU16M
                0xfb -> pure EI
                0xfe -> CP_u8 <$> fetchByteM
                0xff -> pure $ RST Rst38
                unknown -> error $ "found invalid opcode " <> toHex unknown
            pure $ Instruction cycles instr

fetchPrefixed :: GameBoy Instruction
fetchPrefixed = do
    s <- get
    let
        counter = s.registers.pc
        bus = s.memoryBus
    advance 1
    let
        n = readByte bus counter
        cycles = lookupCyclesPrefixed n
    pure $ Instruction cycles $ case n of
        0x00 -> RLC B
        0x01 -> RLC C
        0x02 -> RLC D
        0x03 -> RLC E
        0x04 -> RLC H
        0x05 -> RLC L
        0x06 -> RLC_derefHL
        0x07 -> RLC A
        0x08 -> RRC B
        0x09 -> RRC C
        0x0a -> RRC D
        0x0b -> RRC E
        0x0c -> RRC H
        0x0d -> RRC L
        0x0e -> RRC_derefHL
        0x0f -> RRC A
        0x10 -> RL B
        0x11 -> RL C
        0x12 -> RL D
        0x13 -> RL E
        0x14 -> RL H
        0x15 -> RL L
        0x16 -> RL_derefHL
        0x17 -> RL A
        0x18 -> RR B
        0x19 -> RR C
        0x1a -> RR D
        0x1b -> RR E
        0x1c -> RR H
        0x1d -> RR L
        0x1e -> RR_derefHL
        0x1f -> RR A
        0x20 -> SLA B
        0x21 -> SLA C
        0x22 -> SLA D
        0x23 -> SLA E
        0x24 -> SLA H
        0x25 -> SLA L
        0x26 -> SLA_derefHL
        0x27 -> SLA A
        0x28 -> SRA B
        0x29 -> SRA C
        0x2a -> SRA D
        0x2b -> SRA E
        0x2c -> SRA H
        0x2d -> SRA L
        0x2e -> SRA_derefHL
        0x2f -> SRA A
        0x30 -> SWAP B
        0x31 -> SWAP C
        0x32 -> SWAP D
        0x33 -> SWAP E
        0x34 -> SWAP H
        0x35 -> SWAP L
        0x36 -> SWAP_derefHL
        0x37 -> SWAP A
        0x38 -> SRL B
        0x39 -> SRL C
        0x3a -> SRL D
        0x3b -> SRL E
        0x3c -> SRL H
        0x3d -> SRL L
        0x3e -> SRL_derefHL
        0x3f -> SRL A
        0x40 -> BIT 0 B
        0x41 -> BIT 0 C
        0x42 -> BIT 0 D
        0x43 -> BIT 0 E
        0x44 -> BIT 0 H
        0x45 -> BIT 0 L
        0x46 -> BIT_n_derefHL 0
        0x47 -> BIT 0 A
        0x48 -> BIT 1 B
        0x49 -> BIT 1 C
        0x4a -> BIT 1 D
        0x4b -> BIT 1 E
        0x4c -> BIT 1 H
        0x4d -> BIT 1 L
        0x4e -> BIT_n_derefHL 1
        0x4f -> BIT 1 A
        0x50 -> BIT 2 B
        0x51 -> BIT 2 C
        0x52 -> BIT 2 D
        0x53 -> BIT 2 E
        0x54 -> BIT 2 H
        0x55 -> BIT 2 L
        0x56 -> BIT_n_derefHL 2
        0x57 -> BIT 2 A
        0x58 -> BIT 3 B
        0x59 -> BIT 3 C
        0x5a -> BIT 3 D
        0x5b -> BIT 3 E
        0x5c -> BIT 3 H
        0x5d -> BIT 3 L
        0x5e -> BIT_n_derefHL 3
        0x5f -> BIT 3 A
        0x60 -> BIT 4 B
        0x61 -> BIT 4 C
        0x62 -> BIT 4 D
        0x63 -> BIT 4 E
        0x64 -> BIT 4 H
        0x65 -> BIT 4 L
        0x66 -> BIT_n_derefHL 4
        0x67 -> BIT 4 A
        0x68 -> BIT 5 B
        0x69 -> BIT 5 C
        0x6a -> BIT 5 D
        0x6b -> BIT 5 E
        0x6c -> BIT 5 H
        0x6d -> BIT 5 L
        0x6e -> BIT_n_derefHL 5
        0x6f -> BIT 5 A
        0x70 -> BIT 6 B
        0x71 -> BIT 6 C
        0x72 -> BIT 6 D
        0x73 -> BIT 6 E
        0x74 -> BIT 6 H
        0x75 -> BIT 6 L
        0x76 -> BIT_n_derefHL 6
        0x77 -> BIT 6 A
        0x78 -> BIT 7 B
        0x79 -> BIT 7 C
        0x7a -> BIT 7 D
        0x7b -> BIT 7 E
        0x7c -> BIT 7 H
        0x7d -> BIT 7 L
        0x7e -> BIT_n_derefHL 7
        0x7f -> BIT 7 A
        0x80 -> RES 0 B
        0x81 -> RES 0 C
        0x82 -> RES 0 D
        0x83 -> RES 0 E
        0x84 -> RES 0 H
        0x85 -> RES 0 L
        0x86 -> RES_derefHL 0
        0x87 -> RES 0 A
        0x88 -> RES 1 B
        0x89 -> RES 1 C
        0x8a -> RES 1 D
        0x8b -> RES 1 E
        0x8c -> RES 1 H
        0x8d -> RES 1 L
        0x8e -> RES_derefHL 1
        0x8f -> RES 1 A
        0x90 -> RES 2 B
        0x91 -> RES 2 C
        0x92 -> RES 2 D
        0x93 -> RES 2 E
        0x94 -> RES 2 H
        0x95 -> RES 2 L
        0x96 -> RES_derefHL 2
        0x97 -> RES 2 A
        0x98 -> RES 3 B
        0x99 -> RES 3 C
        0x9a -> RES 3 D
        0x9b -> RES 3 E
        0x9c -> RES 3 H
        0x9d -> RES 3 L
        0x9e -> RES_derefHL 3
        0x9f -> RES 3 A
        0xa0 -> RES 4 B
        0xa1 -> RES 4 C
        0xa2 -> RES 4 D
        0xa3 -> RES 4 E
        0xa4 -> RES 4 H
        0xa5 -> RES 4 L
        0xa6 -> RES_derefHL 4
        0xa7 -> RES 4 A
        0xa8 -> RES 5 B
        0xa9 -> RES 5 C
        0xaa -> RES 5 D
        0xab -> RES 5 E
        0xac -> RES 5 H
        0xad -> RES 5 L
        0xae -> RES_derefHL 5
        0xaf -> RES 5 A
        0xb0 -> RES 6 B
        0xb1 -> RES 6 C
        0xb2 -> RES 6 D
        0xb3 -> RES 6 E
        0xb4 -> RES 6 H
        0xb5 -> RES 6 L
        0xb6 -> RES_derefHL 6
        0xb7 -> RES 6 A
        0xb8 -> RES 7 B
        0xb9 -> RES 7 C
        0xba -> RES 7 D
        0xbb -> RES 7 E
        0xbc -> RES 7 H
        0xbd -> RES 7 L
        0xbe -> RES_derefHL 7
        0xbf -> RES 7 A
        0xc0 -> SET 0 B
        0xc1 -> SET 0 C
        0xc2 -> SET 0 D
        0xc3 -> SET 0 E
        0xc4 -> SET 0 H
        0xc5 -> SET 0 L
        0xc6 -> SET_derefHL 0
        0xc7 -> SET 0 A
        0xc8 -> SET 1 B
        0xc9 -> SET 1 C
        0xca -> SET 1 D
        0xcb -> SET 1 E
        0xcc -> SET 1 H
        0xcd -> SET 1 L
        0xce -> SET_derefHL 1
        0xcf -> SET 1 A
        0xd0 -> SET 2 B
        0xd1 -> SET 2 C
        0xd2 -> SET 2 D
        0xd3 -> SET 2 E
        0xd4 -> SET 2 H
        0xd5 -> SET 2 L
        0xd6 -> SET_derefHL 2
        0xd7 -> SET 2 A
        0xd8 -> SET 3 B
        0xd9 -> SET 3 C
        0xda -> SET 3 D
        0xdb -> SET 3 E
        0xdc -> SET 3 H
        0xdd -> SET 3 L
        0xde -> SET_derefHL 3
        0xdf -> SET 3 A
        0xe0 -> SET 4 B
        0xe1 -> SET 4 C
        0xe2 -> SET 4 D
        0xe3 -> SET 4 E
        0xe4 -> SET 4 H
        0xe5 -> SET 4 L
        0xe6 -> SET_derefHL 4
        0xe7 -> SET 4 A
        0xe8 -> SET 5 B
        0xe9 -> SET 5 C
        0xea -> SET 5 D
        0xeb -> SET 5 E
        0xec -> SET 5 H
        0xed -> SET 5 L
        0xee -> SET_derefHL 5
        0xef -> SET 5 A
        0xf0 -> SET 6 B
        0xf1 -> SET 6 C
        0xf2 -> SET 6 D
        0xf3 -> SET 6 E
        0xf4 -> SET 6 H
        0xf5 -> SET 6 L
        0xf6 -> SET_derefHL 6
        0xf7 -> SET 6 A
        0xf8 -> SET 7 B
        0xf9 -> SET 7 C
        0xfa -> SET 7 D
        0xfb -> SET 7 E
        0xfc -> SET 7 H
        0xfd -> SET 7 L
        0xfe -> SET_derefHL 7
        0xff -> SET 7 A
        unknown -> error $ "impossible prefixed byte: " <> toHex unknown

-- FIXME: to get rid of this, the module structure needs to be reworked and
-- writeByte should/could be done with a side-effect
writeByteM :: U16 -> U8 -> GameBoy ()
writeByteM addr n =
    -- HACK: "listen" for changes that potentially cascade to other state
    -- changes here
    if
        | addr == 0xff46 ->
            dmaTransfer n
        | addr == 0xff07 -> do
            freq <- timerFrequency <$> busM
            modifyBusM (writeByte addr n)
            freq' <- timerFrequency <$> busM
            when (freq' /= freq) $
                setTimerCounterM (counterFromFrequency freq')
        | addr >= 0x2000 && addr < 0x4000 ->
            switchMemoryBank n
        | addr == 0xff50 && n > 0 ->
            unloadBios
        | otherwise ->
            modifyBusM (writeByte addr n)

unloadBios :: GameBoy ()
unloadBios = do
    cart <- ask
    modifyBusM $ \bus -> bus{cartridge = getMemoryBank 0 cart.memory}

-- FIXME: try memorizing the currently used bank and only switch if necessary
switchMemoryBank :: U8 -> GameBoy ()
switchMemoryBank n = do
    cartridge <- ask
    let val = n .&. 0x1f
    modifyBusM $ \bus -> bus{romBank = getMemoryBank val cartridge.memory}

push :: U16 -> GameBoy ()
push n = do
    s <- get
    let curr = s.registers.sp
    modifyStackPointerM (\x -> x - 2)
    writeByteM (curr - 1) hi
    writeByteM (curr - 2) lo
  where
    (hi, lo) = splitIntoBytes n

pop :: GameBoy U16
pop = do
    s <- get
    modifyStackPointerM (+ 2)
    pure $ readU16 s.memoryBus s.registers.sp

dec :: TargetRegister -> GameBoy ()
dec r = modifyRegistersM $ \rs ->
    let
        old = readRegister r rs
        result = old - 1
    in
        rs
            & adjustFlag Zero (result == 0)
                . setFlag Negative
                . adjustFlag HalfCarry (old .&. 0x0f == 0)
                . setRegister r result

inc :: TargetRegister -> GameBoy ()
inc r = modifyRegistersM $ \rs ->
    let
        old = readRegister r rs
        result = old + 1
    in
        rs
            & adjustFlag Zero (result == 0)
                . clearFlag Negative
                . adjustFlag HalfCarry (old .&. 0x0f == 0x0f)
                . setRegister r result

ld_r_r :: TargetRegister -> TargetRegister -> GameBoy ()
ld_r_r r r' = modifyRegistersM $ \rs ->
    let val = readRegister r' rs
    in setRegister r val rs

deref :: TargetRegister16 -> GameBoy U8
deref rr = do
    s <- get
    pure $ readByte s.memoryBus (readRegister16 rr s.registers)

execute :: Instruction -> GameBoy Int
execute Instruction{tag, baseCycles} =
    case tag of
        NOP ->
            pure baseCycles
        LD_u16 rr n ->
            exec $ setRegister16M rr n
        LD_deref_rr_A rr -> exec $ do
            rs <- registersM
            writeByteM (readRegister16 rr rs) rs.a
        LDSP_u16 n ->
            exec $ modifyStackPointerM (const n)
        LD_u16SP n -> exec $ do
            stackPointer <- gets (.registers.sp)
            let (hi, lo) = splitIntoBytes stackPointer
            writeByteM n lo
            writeByteM (n + 1) hi
        LD_r_HLderef r -> exec $ do
            n <- deref HL
            setRegisterM r n
        LD_HLminus_A -> exec $ do
            rs <- registersM
            writeByteM (hl rs) rs.a
            modifyRegister16M HL (\x -> x - 1)
        LD_HLplus_A -> exec $ do
            rs <- registersM
            writeByteM (hl rs) rs.a
            modifyRegister16M HL (+ 1)
        LD_HLderef_u8 n -> exec $ do
            rs <- registersM
            writeByteM (hl rs) n
        LD_A_deref rr -> exec $ do
            n <- deref rr
            setRegisterM A n
        LD_A_HLplus -> exec $ do
            n <- deref HL
            modifyRegistersM $ setRegister A n . modifyRegister16 HL (+ 1)
        LD_A_HLminus -> exec $ do
            n <- deref HL
            modifyRegistersM $ setRegister A n . modifyRegister16 HL (\x -> x - 1)
        LD_A_FF00plusU8 n -> exec $ do
            modify' $ \s ->
                setRegister' A (readByte s.memoryBus (0xff00 + fromIntegral n)) s
        LD_A_FF00plusC -> exec $ do
            modify' $ \s ->
                let offset = s.registers.c
                in setRegister' A (readByte s.memoryBus (0xff00 + fromIntegral offset)) s
        LD_A_derefU16 n -> exec $ do
            modify' $ \s ->
                setRegister' A (readByte s.memoryBus n) s
        LD_u8 r n ->
            exec $ setRegisterM r n
        LD r r' ->
            exec $ ld_r_r r r'
        LD_u16_A n -> exec $ do
            rs <- registersM
            writeByteM n rs.a
        LD_FF00plusC_A -> exec $ do
            s <- get
            let offset = fromIntegral s.registers.c
            writeByteM (0xff00 + offset) s.registers.a
        LD_FF00plusU8_A n -> exec $ do
            s <- get
            writeByteM (0xff00 + fromIntegral n) s.registers.a
        LD_derefHL r -> exec $ do
            rs <- registersM
            writeByteM (hl rs) (readRegister r rs)
        LDSP_HL -> exec $ do
            modifyRegistersM $ \rs ->
                modifyStackPointer (const $ hl rs) rs
        LD_HLSP n -> exec $ do
            modifyRegistersM $ \rs ->
                let
                    orig = rs.sp
                    res' = fromIntegral @_ @I32 orig + fromIntegral n
                    res = fromIntegral res'
                    needsHalfCarry = toU8 orig .&. 0xf + toU8 n .&. 0xf > 0xf
                    needsCarry = orig .&. 0xff + toU16 (toU8 n) > 0xff
                in
                    rs
                        & setRegister16 HL res
                        & clearFlag Zero
                        & clearFlag Negative
                        & adjustFlag HalfCarry needsHalfCarry
                        & adjustFlag Carry needsCarry
        BIT n r -> exec $ do
            modifyRegistersM $ \rs ->
                let bitIsSet = Bits.testBit (readRegister r rs) n
                in rs
                    & setFlag HalfCarry
                        . clearFlag Negative
                        . adjustFlag Zero (not bitIsSet)
        BIT_n_derefHL n -> exec $ do
            val <- deref HL
            let bitIsSet = Bits.testBit val n
            modifyRegistersM $ \rs ->
                rs
                    & setFlag HalfCarry
                        . clearFlag Negative
                        . adjustFlag Zero (not bitIsSet)
        JR n ->
            exec $ modifyProgramCounterM (+ fromIntegral n)
        JR_cc cond n -> do
            jump <- gets (checkFlagCondition cond)
            if jump
                then do
                    modifyProgramCounterM (+ fromIntegral n)
                    pure $ baseCycles + 4
                else pure baseCycles
        JP_cc cond n -> do
            jump <- gets (checkFlagCondition cond)
            if jump
                then do
                    jumpM n
                    pure $ baseCycles + 4
                else pure $ baseCycles
        RET -> exec $ do
            addr <- pop
            jumpM addr
        RET_cc cond -> do
            jump <- gets (checkFlagCondition cond)
            if jump
                then do
                    addr <- pop
                    jumpM addr
                    pure 20
                else pure baseCycles
        RETI -> exec $ do
            addr <- pop
            jumpM addr
            enableInterruptsM
        CALL n -> exec $ do
            counter <- programCounterM
            push counter
            jumpM n
        CALL_cc cond n -> do
            s <- get
            if (checkFlagCondition cond s)
                then do
                    let counter = s.registers.pc
                    push counter
                    jumpM n
                    pure 24
                else pure baseCycles
        JP n ->
            exec $ jumpM n
        JP_HL -> exec $ do
            modifyRegistersM $ \rs ->
                modifyProgramCounter (const $ hl rs) rs
        INC r ->
            exec $ inc r
        INC_derefHL -> exec $ do
            s <- get
            let
                addr = hl s.registers
                orig = readByte s.memoryBus addr
                val = orig + 1
            writeByteM addr val
            modifyRegistersM $
                adjustFlag Zero (val == 0)
                    . clearFlag Negative
                    . adjustFlag HalfCarry (val .&. 0xf == 0)
        INC16 rr ->
            exec $ modifyRegister16M rr (+ 1)
        INCSP ->
            exec $ modifyStackPointerM (+ 1)
        DEC r ->
            exec $ dec r
        DEC16 rr ->
            exec $ modifyRegister16M rr (\n -> n - 1)
        DECSP ->
            exec $ modifyStackPointerM (\x -> x - 1)
        DEC_derefHL -> exec $ do
            s <- get
            let
                addr = hl s.registers
                val = readByte s.memoryBus addr - 1
            writeByteM addr val
            modifyRegistersM
                ( adjustFlag Zero (val == 0)
                    . setFlag Negative
                    . adjustFlag HalfCarry ((val + 1) .&. 0x0f == 0)
                )
        PUSH rr -> exec $ do
            n <- readRegister16M rr
            push n
        PUSH_AF -> exec $ do
            rs <- registersM
            push (af rs)
        POP rr -> exec $ do
            n <- pop
            setRegister16M rr n
        POP_AF -> exec $ do
            n <- pop
            modifyRegistersM $ setAF n
        RLA -> exec $ do
            modifyRegistersM $ \rs ->
                let
                    orig = rs.a
                    carry = if hasFlag Carry rs then 1 else 0
                    carry' = Bits.testBit orig 7
                    res = Bits.shiftL orig 1 + carry
                in
                    rs{a = res}
                        & adjustFlag Carry carry'
                            . clearFlag Zero
                            . clearFlag HalfCarry
                            . clearFlag Negative
        RRA -> exec $ do
            modifyRegistersM $ \rs ->
                let
                    orig = rs.a
                    carry = if hasFlag Carry rs then 1 else 0
                    carry' = Bits.testBit orig 0
                    res = Bits.shiftR orig 1 + Bits.shiftL carry 7
                in
                    rs{a = res}
                        & adjustFlag Carry carry'
                            . clearFlag Zero
                            . clearFlag HalfCarry
                            . clearFlag Negative
        DI ->
            exec disableInterruptsM
        EI ->
            exec enableInterruptsM
        OR r ->
            exec $ or_a r
        OR_u8 n ->
            exec $ or_a_u8 n
        OR_A_HL ->
            exec $ or_a_u8 =<< deref HL
        AND r ->
            exec $ and_a r
        AND_u8 n ->
            exec $ and_a_u8 n
        AND_A_HL ->
            exec $ and_a_u8 =<< deref HL
        ADD r ->
            exec $ add_a r
        ADD_u8 n ->
            exec $ add_a_u8 n
        ADD_A_HL ->
            exec $ add_a_u8 =<< deref HL
        ADD_HL rr ->
            exec $ add_hl rr
        ADD_HLSP ->
            exec $ add_hlsp -- TODO: refactor
        ADD_SP n ->
            exec $ add_sp n
        SUB r ->
            exec $ sub_a r
        SUB_u8 n ->
            exec $ sub_a_u8 n
        SUB_A_HL ->
            exec $ sub_a_u8 =<< deref HL
        ADC r ->
            exec $ adc_a r
        ADC_u8 n ->
            exec $ adc_a_u8 n
        ADC_A_HL ->
            exec $ adc_a_u8 =<< deref HL
        SBC r ->
            exec $ sbc_a r
        SBC_u8 n ->
            exec $ sbc_a_u8 n
        SBC_A_HL ->
            exec $ sbc_a_u8 =<< deref HL
        CP r ->
            exec $ cp_a r
        CP_u8 n ->
            exec $ cp_a_u8 n
        CP_A_HL ->
            exec $ cp_a_u8 =<< deref HL
        XOR r ->
            exec $ xor_a r
        XOR_u8 n ->
            exec $ xor_a_u8 n
        XOR_A_HL ->
            exec $ xor_a_u8 =<< deref HL
        RST addr -> exec $ do
            counter <- programCounterM
            push counter
            jumpM (getRestartAddr addr)
        CPL -> exec $ do
            modifyRegistersM $ \rs ->
                rs
                    & modifyRegister A Bits.complement
                    & setFlag Negative
                    & setFlag HalfCarry
        SWAP r -> exec $ modifyRegistersM $ \rs ->
            let
                orig = readRegister r rs
                res = Bits.rotate orig 4
            in
                rs
                    & setRegister r res
                        . clearFlag Negative
                        . clearFlag HalfCarry
                        . clearFlag Carry
                        . adjustFlag Zero (res == 0)
        SWAP_derefHL -> exec $ do
            s <- get
            let
                rs = s.registers
                addr = hl rs
                orig = readByte s.memoryBus addr
                res = Bits.rotate orig 4
            writeByteM addr res
            modifyRegistersM
                ( clearFlag Negative
                    . clearFlag HalfCarry
                    . clearFlag Carry
                    . adjustFlag Zero (res == 0)
                )
        RES n r ->
            exec $ modifyRegisterM r (`Bits.clearBit` n)
        RES_derefHL n -> exec $ do
            s <- get
            let
                rs = s.registers
                addr = hl rs
                val = readByte s.memoryBus addr
                res = Bits.clearBit val n
            writeByteM addr res
        SET n r ->
            exec $ modifyRegisterM r (`Bits.setBit` n)
        SET_derefHL n -> exec $ do
            s <- get
            let
                rs = s.registers
                addr = hl rs
                val = readByte s.memoryBus addr
                res = Bits.setBit val n
            writeByteM addr res
        SRL r -> exec $ modifyRegistersM $ \rs ->
            let
                orig = readRegister r rs
                res = orig .>>. 1
            in
                rs
                    & setRegister r res
                    & clearFlag Negative
                    & clearFlag HalfCarry
                    & adjustFlag Carry (Bits.testBit orig 0)
                    & adjustFlag Zero (res == 0)
        SRL_derefHL -> exec $ do
            s <- get
            let
                addr = hl s.registers
                orig = readByte s.memoryBus addr
                res = orig .>>. 1
            writeByteM addr res
            modifyRegistersM $
                clearFlag Negative
                    . clearFlag HalfCarry
                    . adjustFlag Carry (Bits.testBit orig 0)
                    . adjustFlag Zero (res == 0)
        RR r -> exec $ do
            modifyRegistersM $ \rs ->
                let
                    orig = readRegister r rs
                    carry = if hasFlag Carry rs then 1 else 0
                    carry' = Bits.testBit orig 0
                    res = Bits.shiftR orig 1 + Bits.shiftL carry 7
                in
                    rs
                        & adjustFlag Carry carry'
                            . adjustFlag Zero (res == 0)
                            . clearFlag HalfCarry
                            . clearFlag Negative
                            . setRegister r res
        RR_derefHL -> exec $ do
            s <- get
            let
                addr = hl s.registers
                carry = if hasFlag' Carry s then 1 else 0
                orig = readByte s.memoryBus addr
                carry' = Bits.testBit orig 0
                res = Bits.shiftR orig 1 + Bits.shiftL carry 7
            writeByteM addr res
            modifyRegistersM $
                adjustFlag Carry carry'
                    . adjustFlag Zero (res == 0)
                    . clearFlag HalfCarry
                    . clearFlag Negative
        RRC r -> exec $ do
            modifyRegistersM $ \rs ->
                let
                    orig = readRegister r rs
                    carry = Bits.testBit orig 0
                    res = Bits.rotateR orig 1
                in
                    rs
                        & adjustFlag Carry carry
                            . adjustFlag Zero (res == 0)
                            . clearFlag HalfCarry
                            . clearFlag Negative
                            . setRegister r res
        RRC_derefHL -> exec $ do
            s <- get
            let
                addr = hl s.registers
                orig = readByte s.memoryBus addr
                carry = Bits.testBit orig 0
                res = Bits.rotateR orig 1
            writeByteM addr res
            modifyRegistersM $
                adjustFlag Carry carry
                    . adjustFlag Zero (res == 0)
                    . clearFlag HalfCarry
                    . clearFlag Negative
        RL r -> exec $ do
            modifyRegistersM $ \rs ->
                let
                    orig = readRegister r rs
                    carry = if hasFlag Carry rs then 1 else 0
                    carry' = Bits.testBit orig 7
                    res = Bits.shiftL orig 1 + carry
                in
                    rs
                        & adjustFlag Carry carry'
                            . adjustFlag Zero (res == 0)
                            . clearFlag HalfCarry
                            . clearFlag Negative
                            . setRegister r res
        RL_derefHL -> exec $ do
            s <- get
            let
                addr = hl s.registers
                orig = readByte s.memoryBus addr
                carry = if hasFlag' Carry s then 1 else 0
                carry' = Bits.testBit orig 7
                res = Bits.shiftL orig 1 + carry
            writeByteM addr res
            modifyRegistersM $
                adjustFlag Carry carry'
                    . adjustFlag Zero (res == 0)
                    . clearFlag HalfCarry
                    . clearFlag Negative
        RLC r -> exec $ do
            modifyRegistersM $ \rs ->
                let
                    orig = readRegister r rs
                    carry = Bits.testBit orig 7
                    res = Bits.rotateL orig 1
                in
                    rs
                        & adjustFlag Carry carry
                            . adjustFlag Zero (res == 0)
                            . clearFlag HalfCarry
                            . clearFlag Negative
                            . setRegister r res
        RLC_derefHL -> exec $ do
            s <- get
            let
                addr = hl s.registers
                orig = readByte s.memoryBus addr
                carry = Bits.testBit orig 7
                res = Bits.rotateL orig 1
            writeByteM addr res
            modifyRegistersM $
                adjustFlag Carry carry
                    . adjustFlag Zero (res == 0)
                    . clearFlag HalfCarry
                    . clearFlag Negative
        SLA r -> exec $ do
            modifyRegistersM $ \rs ->
                let
                    orig = readRegister r rs
                    carry = Bits.testBit orig 7
                    res = Bits.shiftL orig 1
                in
                    rs
                        & adjustFlag Carry carry
                            . adjustFlag Zero (res == 0)
                            . clearFlag HalfCarry
                            . clearFlag Negative
                            . setRegister r res
        SLA_derefHL -> exec $ do
            s <- get
            let
                addr = hl s.registers
                orig = readByte s.memoryBus addr
                carry = Bits.testBit orig 7
                res = Bits.shiftL orig 1
            writeByteM addr res
            modifyRegistersM $
                adjustFlag Carry carry
                    . adjustFlag Zero (res == 0)
                    . clearFlag HalfCarry
                    . clearFlag Negative
        SRA r -> exec $ do
            modifyRegistersM $ \rs ->
                let
                    orig = readRegister r rs
                    carry = Bits.testBit orig 0
                    msb = orig .&. 0x80
                    res = Bits.shiftR orig 1 + msb
                in
                    rs
                        & adjustFlag Carry carry
                            . adjustFlag Zero (res == 0)
                            . clearFlag HalfCarry
                            . clearFlag Negative
                            . setRegister r res
        SRA_derefHL -> exec $ do
            s <- get
            let
                addr = hl s.registers
                orig = readByte s.memoryBus addr
                carry = Bits.testBit orig 0
                msb = orig .&. 0x80
                res = Bits.shiftR orig 1 + msb
            writeByteM addr res
            modifyRegistersM $
                adjustFlag Carry carry
                    . adjustFlag Zero (res == 0)
                    . clearFlag HalfCarry
                    . clearFlag Negative
        DAA ->
            exec $ daa
        SCF -> exec $ do
            modifyRegistersM $ \rs ->
                rs
                    & clearFlag Negative
                        . clearFlag HalfCarry
                        . setFlag Carry
        CCF -> exec $ do
            modifyRegistersM $ \rs ->
                rs
                    & clearFlag Negative
                        . clearFlag HalfCarry
                        . adjustFlag Carry (not $ hasFlag Carry rs)
        RLCA -> exec $ do
            modifyRegistersM $ \rs ->
                let
                    orig = rs.a
                    res = Bits.rotateL orig 1
                    needsCarry = Bits.testBit orig 7
                in
                    rs{a = res}
                        & clearFlag Zero
                            . clearFlag Negative
                            . clearFlag HalfCarry
                            . adjustFlag Carry needsCarry
        RRCA -> exec $ do
            modifyRegistersM $ \rs ->
                let
                    orig = rs.a
                    res = Bits.rotateR orig 1
                    needsCarry = Bits.testBit orig 0
                in
                    rs{a = res}
                        & clearFlag Zero
                            . clearFlag Negative
                            . clearFlag HalfCarry
                            . adjustFlag Carry needsCarry
        HALT -> do
            halt
            pure 0
        STOP -> do
            halt
            pure 0
  where
    exec action = action >> pure baseCycles

halt :: GameBoy ()
halt = modify' $ \s -> s{halted = True}

continue :: GameBoy ()
continue = modify' $ \s -> s{halted = False}

toggleInterruptsM :: Bool -> GameBoy ()
toggleInterruptsM enabled = modify' $ \s ->
    s{masterInterruptEnable = enabled}

enableInterruptsM :: GameBoy ()
enableInterruptsM = toggleInterruptsM True

disableInterruptsM :: GameBoy ()
disableInterruptsM = toggleInterruptsM False

add_sp :: I8 -> GameBoy ()
add_sp n = modifyRegistersM $ \rs ->
    let (res, needsHalfCarry, needsCarry) = add_spPure rs.sp n
    in rs{sp = res}
        & clearFlag Negative
            . clearFlag Zero
            . adjustFlag HalfCarry needsHalfCarry
            . adjustFlag Carry needsCarry

add_spPure :: U16 -> I8 -> (U16, Bool, Bool)
add_spPure orig n =
    let
        n' = toU8 n
        res' = fromIntegral @_ @I32 orig + fromIntegral n
        needsHalfCarry = toU8 (orig .&. 0xf) + n' .&. 0xf > 0xf
        needsCarry = orig .&. 0xff + toU16 n' > 0xff
    in
        (fromIntegral res', needsHalfCarry, needsCarry)

daa :: GameBoy ()
daa = modifyRegistersM $ \rs ->
    let
        orig = rs.a
        halfCarry = hasFlag HalfCarry rs
        carry = hasFlag Carry rs
        negative = hasFlag Negative rs
        u =
            if halfCarry || (not negative && (orig .&. 0xf) > 9)
                then 6
                else 0
        (u', setCarry) =
            if carry || not negative && orig > 0x99
                then (u .|. 0x60, True)
                else (u, False)
        res = orig + if negative then (-u') else u'
    in
        rs{a = res}
            & adjustFlag Zero (res == 0)
                . adjustFlag Carry setCarry
                . clearFlag HalfCarry

add_hl :: TargetRegister16 -> GameBoy ()
add_hl rr = modifyRegistersM $ \rs ->
    let
        orig = hl rs
        val = readRegister16 rr rs
        res' = fromIntegral @_ @U32 orig + fromIntegral val
        res = fromIntegral res'
        needsCarry = res' > 0xffff
        needsHalfCarry = fromIntegral val .&. 0xfff > res' .&. 0xfff
    in
        rs
            & setRegister16 HL res
            & clearFlag Negative
            & adjustFlag HalfCarry needsHalfCarry
            & adjustFlag Carry needsCarry

add_hlsp :: GameBoy ()
add_hlsp = modifyRegistersM $ \rs ->
    let
        orig = hl rs
        val = rs.sp
        res' = fromIntegral @_ @U32 orig + fromIntegral val
        res = fromIntegral res'
        needsCarry = res' > 0xffff
        needsHalfCarry = fromIntegral val .&. 0xfff > res' .&. 0xfff
    in
        rs
            & setRegister16 HL res
            & clearFlag Negative
            & adjustFlag HalfCarry needsHalfCarry
            & adjustFlag Carry needsCarry

or_a :: TargetRegister -> GameBoy ()
or_a r = modifyRegistersM $ \rs ->
    let
        val = readRegister r rs
        res = rs.a .|. val
    in
        rs{a = res}
            & clearFlag Negative
                . clearFlag Carry
                . clearFlag HalfCarry
                . adjustFlag Zero (res == 0)

or_a_u8 :: U8 -> GameBoy ()
or_a_u8 n = modifyRegistersM $ \rs ->
    let res = rs.a .|. n
    in rs{a = res}
        & clearFlag Negative
            . clearFlag Carry
            . clearFlag HalfCarry
            . adjustFlag Zero (res == 0)

and_a :: TargetRegister -> GameBoy ()
and_a r = modifyRegistersM $ \rs ->
    let
        val = readRegister r rs
        res = rs.a .&. val
    in
        rs{a = res}
            & clearFlag Negative
                . clearFlag Carry
                . setFlag HalfCarry
                . adjustFlag Zero (res == 0)

and_a_u8 :: U8 -> GameBoy ()
and_a_u8 n = modifyRegistersM $ \rs ->
    let res = rs.a .&. n
    in rs{a = res}
        & clearFlag Negative
            . clearFlag Carry
            . setFlag HalfCarry
            . adjustFlag Zero (res == 0)

add_a :: TargetRegister -> GameBoy ()
add_a r = modifyRegistersM $ \rs ->
    let
        orig = rs.a
        val = readRegister r rs
        res' = toU16 orig + toU16 val
        res = fromIntegral res'
        needsHalfCarry = (orig .&. 0x0f) + (val .&. 0x0f) > 0x0f
        needsCarry = res' > 0xff
    in
        rs{a = res}
            & clearFlag Negative
                . adjustFlag Carry needsCarry
                . adjustFlag HalfCarry needsHalfCarry
                . adjustFlag Zero (res == 0)

add_a_u8 :: U8 -> GameBoy ()
add_a_u8 n = do
    modifyRegistersM $ \rs ->
        let
            orig = rs.a
            res' = toU16 orig + toU16 n
            res = fromIntegral res'
            needsHalfCarry = (orig .&. 0x0f) + (n .&. 0x0f) > 0x0f
            needsCarry = res' > 0xff
        in
            rs{a = res}
                & clearFlag Negative
                    . adjustFlag Carry needsCarry
                    . adjustFlag HalfCarry needsHalfCarry
                    . adjustFlag Zero (res == 0)

xor_a :: TargetRegister -> GameBoy ()
xor_a r = modifyRegistersM $ \rs ->
    let
        val = readRegister r rs
        res = rs.a `Bits.xor` val
    in
        rs{a = res}
            & clearFlag Negative
                . clearFlag Carry
                . clearFlag HalfCarry
                . adjustFlag Zero (res == 0)

xor_a_u8 :: U8 -> GameBoy ()
xor_a_u8 n = modifyRegistersM $ \rs ->
    let res = rs.a `Bits.xor` n
    in rs{a = res}
        & adjustFlag Zero (res == 0)
        & clearFlag Negative
        & clearFlag HalfCarry
        & clearFlag Carry

cp_a :: TargetRegister -> GameBoy ()
cp_a r = modifyRegistersM $ \rs ->
    let
        orig = rs.a
        val = readRegister r rs
        needsHalfCarry = orig .&. 0x0f < val .&. 0x0f
        res = orig - val
    in
        rs
            & setFlag Negative
                . adjustFlag Carry (orig < val)
                . adjustFlag HalfCarry needsHalfCarry
                . adjustFlag Zero (res == 0)

cp_a_u8 :: U8 -> GameBoy ()
cp_a_u8 n = modifyRegistersM $ \rs ->
    let
        orig = rs.a
        res = orig - n
        needsHalfCarry = orig .&. 0x0f < n .&. 0x0f
    in
        rs
            & adjustFlag Zero (res == 0)
                . setFlag Negative
                . adjustFlag HalfCarry needsHalfCarry
                . adjustFlag Carry (orig < n)

sbc_a :: TargetRegister -> GameBoy ()
sbc_a r = modifyRegistersM $ \rs ->
    let
        orig = rs.a
        carry = if hasFlag Carry rs then 1 else 0
        n = readRegister r rs
        val = n + carry
        res = orig - val
        needsHalfCarry = toI16 orig .&. 0xf - toI16 n .&. 0xf - toI16 carry < 0
        needsCarry = toI16 orig - toI16 n - toI16 carry < 0
    in
        rs{a = res}
            & setFlag Negative
                . adjustFlag Carry needsCarry
                . adjustFlag HalfCarry needsHalfCarry
                . adjustFlag Zero (res == 0)

sbc_a_u8 :: U8 -> GameBoy ()
sbc_a_u8 n = modifyRegistersM $ \rs ->
    let
        orig = rs.a
        carry = if hasFlag Carry rs then 1 else 0
        val = n + carry
        res = orig - val
        needsHalfCarry = toI16 orig .&. 0xf - toI16 n .&. 0xf - toI16 carry < 0
        needsCarry = toI16 orig - toI16 n - toI16 carry < 0
    in
        rs{a = res}
            & setFlag Negative
                . adjustFlag Carry needsCarry
                . adjustFlag HalfCarry needsHalfCarry
                . adjustFlag Zero (res == 0)

sub_a :: TargetRegister -> GameBoy ()
sub_a r = modifyRegistersM $ \rs ->
    let
        orig = rs.a
        val = readRegister r rs
        res = rs.a - val
        needsHalfCarry = orig .&. 0x0f < val .&. 0x0f
    in
        rs{a = res}
            & setFlag Negative
                . adjustFlag Carry (orig < val)
                . adjustFlag HalfCarry needsHalfCarry
                . adjustFlag Zero (res == 0)

sub_a_u8 :: U8 -> GameBoy ()
sub_a_u8 n = modifyRegistersM $ \rs ->
    let
        orig = rs.a
        res = orig - n
        needsHalfCarry = orig .&. 0x0f < n .&. 0x0f
    in
        rs{a = res}
            & setFlag Negative
                . adjustFlag Carry (orig < n)
                . adjustFlag HalfCarry needsHalfCarry
                . adjustFlag Zero (res == 0)

adc_a :: TargetRegister -> GameBoy ()
adc_a r = modifyRegistersM $ \rs ->
    let
        orig = rs.a
        val = readRegister r rs
        carry = if hasFlag Carry rs then 1 else 0
        res' = toU16 orig + toU16 val + toU16 carry
        needsCarry = res' > 0xff
        res = fromIntegral res'
        needsHalfCarry = (orig .&. 0x0f) + (val .&. 0x0f) + carry > 0x0f
    in
        rs{a = res}
            & clearFlag Negative
                . adjustFlag Carry needsCarry
                . adjustFlag HalfCarry needsHalfCarry
                . adjustFlag Zero (res == 0)

adc_a_u8 :: U8 -> GameBoy ()
adc_a_u8 n = modifyRegistersM $ \rs ->
    let
        orig = rs.a
        carry = if hasFlag Carry rs then 1 else 0
        res' = toU16 orig + toU16 n + toU16 carry
        needsCarry = res' > 0xff
        res = fromIntegral res'
        needsHalfCarry = (orig .&. 0x0f) + (n .&. 0x0f) + carry > 0x0f
    in
        rs{a = res}
            & clearFlag Negative
                . adjustFlag Carry needsCarry
                . adjustFlag HalfCarry needsHalfCarry
                . adjustFlag Zero (res == 0)

updateTimers :: Int -> GameBoy ()
updateTimers cycles = do
    updateDivider cycles
    s <- get
    when (timerEnable s.memoryBus) $ do
        let counter' = s.timerCounter - cycles
        setTimerCounterM counter'
        when (counter' <= 0) $ do
            freq <- timerFrequency <$> busM
            setTimerCounterM (counterFromFrequency freq)
            if tima s.memoryBus == maxBound
                then do
                    modifyBusM $ modifyTima (const $ tma s.memoryBus)
                    modifyBusM $ requestInterrupt 2
                else modifyBusM $ modifyTima (+ 1)

setTimerCounterM :: Int -> GameBoy ()
setTimerCounterM val =
    modify' $ \s -> s{timerCounter = val}

updateDivider :: Int -> GameBoy ()
updateDivider cycles = do
    counter <- gets (.dividerCounter)
    let counter' = counter + cycles
    setDividerCounter counter'
    when (counter' >= 255) $ do
        setDividerCounter 0
        modifyBusM $ modifyDivider (+ 1)
  where
    setDividerCounter val = modify' $ \s -> s{dividerCounter = val}

data TimerFrequency
    = Freq4K
    | Freq16K
    | Freq64K
    | Freq256K
    deriving (Eq, Show)

readTimerFrequency :: U8 -> TimerFrequency
readTimerFrequency n =
    case (Bits.testBit n 1, Bits.testBit n 0) of
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

timerFrequency :: MemoryBus -> TimerFrequency
timerFrequency = readTimerFrequency . tac

handleInterrupts :: GameBoy Int
handleInterrupts = do
    s <- get
    if s.masterInterruptEnable
        then do
            let
                enabledInterrupts = s.memoryBus.ie
                requestedInterrupts = interruptFlags s.memoryBus
            case findInterrupt (filter (Bits.testBit requestedInterrupts) [0 .. 4]) enabledInterrupts of
                Nothing -> pure 0
                Just interrupt -> do
                    handleInterrupt interrupt
                    pure 20
        else -- TODO: check whether 0 is correct; shouldn't it take cycles to
        -- check memory?
            pure 0
  where
    findInterrupt requestedInterrupts enabledInterrupts =
        case requestedInterrupts of
            [] -> Nothing
            (x : xs) ->
                if Bits.testBit enabledInterrupts x
                    then Just x
                    else findInterrupt xs enabledInterrupts
    handleInterrupt interrupt = do
        -- traceM $ "      INTERRUPT " <> show interrupt
        disableInterruptsM
        modifyBusM $ disableInterrupt interrupt
        counter <- programCounterM
        push counter
        case interrupt of
            0 -> jumpM 0x40 -- VBlank
            1 -> jumpM 0x48 -- LCD stat
            2 -> jumpM 0x50 -- Timer
            3 -> jumpM 0x58 -- Serial
            4 -> jumpM 0x60 -- Joypad
            s -> error $ "unhandled interrupt: " <> show s

dmaTransfer :: U8 -> GameBoy ()
dmaTransfer n = do
    let startAddr :: U16 = Bits.shiftL (fromIntegral n) 8 -- times 0x100
    bus <- busM
    -- TODO: use a slice pointing to cartridge memory instead?
    forM_ [0 .. 0xa0 - 1] $ \i ->
        -- TODO: improve
        writeByteM (0xfe00 + i) (readByte bus (startAddr + i))
