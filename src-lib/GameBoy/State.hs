{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

-- TODO: check whether using StrictData is beneficial once it works

module GameBoy.State where

import Control.Monad.State.Strict
import Data.Bits ((.&.))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Unboxed.Mutable (IOVector)

import GameBoy.BitStuff
import GameBoy.Gamepad

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

bc :: Registers -> U16
bc rs = combineBytes rs.b rs.c

setBC :: U16 -> Registers -> Registers
setBC n rs =
    let (hi, lo) = splitIntoBytes n
    in rs{b = hi, c = lo}

de :: Registers -> U16
de rs = combineBytes rs.d rs.e

setDE :: U16 -> Registers -> Registers
setDE n rs =
    let (hi, lo) = splitIntoBytes n
    in rs{d = hi, e = lo}

hl :: Registers -> U16
hl rs = combineBytes rs.h rs.l

setHL :: U16 -> Registers -> Registers
setHL n rs =
    let (hi, lo) = splitIntoBytes n
    in rs{h = hi, l = lo}

af :: Registers -> U16
af rs = combineBytes rs.a rs.f

setAF :: U16 -> Registers -> Registers
setAF n rs =
    let (hi, lo) = splitIntoBytes (0xfff0 .&. n)
    in rs{a = hi, f = lo}

{- FOURMOLU_DISABLE -}
instance Show Registers where
    show rs = mconcat
        [ "AF = " , toHex (af rs)
        , " | BC = " , toHex (bc rs)
        , " | DE = " , toHex (de rs)
        , " | HL = " , toHex (hl rs)
        , " | PC = " , toHex rs.pc
        , " | SP = " , toHex rs.sp
        ]
{- FOURMOLU_ENABLE -}

type Memory = IOVector U8

data MemoryBus = MemoryBus
    { cartridge :: Memory
    -- ^ Cartridge RAM: $0000 - $7fff
    , vram :: Memory
    -- ^ VRAM: $8000 - $9fff
    , sram :: Memory
    -- ^ SRAM: $a000 - $bfff
    , wram :: Memory
    -- ^ WRAM: $c000 - $dfff
    , oam :: Memory
    -- ^ Object attribute memory: $fe00 - $fe9f
    , gamepadState :: GamepadState
    -- ^ Current state of the buttons: $ff00
    , io :: Memory
    -- ^ IO registers: $ff00 - $ff7f
    , hram :: Memory
    -- ^ HRAM: $ff80 - $fffe
    , ie :: U8
    -- ^ Interrupt register: $ffff
    }

type InMemoryScreen = Vector (Vector U8)

emptyScreen :: InMemoryScreen
emptyScreen = Vector.replicate 144 emptyLine
  where
    emptyLine = Vector.replicate 160 0

data CPUState = CPUState
    { registers :: Registers
    , memoryBus :: MemoryBus
    , dividerCounter :: Int
    , timerCounter :: Int
    , masterInterruptEnable :: Bool
    , scanlineCounter :: Int
    , screen :: InMemoryScreen
    , preparedScreen :: InMemoryScreen
    , halted :: Bool
    }

mkInitialState :: MemoryBus -> CPUState
mkInitialState bus =
    CPUState initialRegisters bus 0 1024 True 456 emptyScreen emptyScreen False
  where
    initialRegisters =
        Registers
            { a = 0x1
            , b = 0
            , c = 0x13
            , d = 0
            , e = 0xd8
            , h = 0x1
            , l = 0x4d
            , f = 0xb0
            , pc = 0x100 -- start without BIOS for now
            , sp = 0xfffe
            }

busM :: GameBoy MemoryBus
busM = gets (.memoryBus)

modifyBusM :: (MemoryBus -> MemoryBus) -> GameBoy ()
modifyBusM fn = modify' $ \s -> s{memoryBus = fn s.memoryBus}

modifyRegistersM :: (Registers -> Registers) -> GameBoy ()
modifyRegistersM fn = modify' $ \s -> s{registers = fn s.registers}

modifyScreenM :: (InMemoryScreen -> InMemoryScreen) -> GameBoy ()
modifyScreenM f = modify' $ \s -> s{screen = f s.screen}

type GameBoy a = StateT CPUState IO a
