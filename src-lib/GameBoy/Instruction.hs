{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module GameBoy.Instruction (
    FlagCondition (..),
    Instr (..),
    Instruction (..),
    RestartAddr (..),
    TargetRegister (..),
    TargetRegister16 (..),
    getRestartAddr,
)
where

import GameBoy.BitStuff
import GameBoy.Cycles

data TargetRegister = A | B | C | D | E | H | L
    deriving stock (Show)

data TargetRegister16 = BC | DE | HL
    deriving stock (Show)

data FlagCondition = ZUnset | ZSet | CUnset | CSet

instance Show FlagCondition where
    show = \case
        ZUnset -> "NZ"
        ZSet -> "Z"
        CUnset -> "NC"
        CSet -> "C"

data RestartAddr
    = Rst00
    | Rst08
    | Rst10
    | Rst18
    | Rst20
    | Rst28
    | Rst30
    | Rst38

instance Show RestartAddr where
    show = toHex . getRestartAddr

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

data Instruction = Instruction
    { baseCycles :: Cycles
    , tag :: Instr
    }

instance Show Instruction where
    show Instruction{tag} = show tag
