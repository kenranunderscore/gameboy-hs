{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

-- TODO: check whether using StrictData is beneficial once it works

module GameBoy.State where

import Data.Bits ((.&.))
import Control.Monad.State.Strict
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Optics

import GameBoy.BitStuff
import GameBoy.Memory

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

bc ::  Registers -> U16
bc rs = combineBytes rs.b rs.c

setBC :: U16 -> Registers  -> Registers
setBC n rs =
    let (hi, lo) = splitIntoBytes n
    in rs{b = hi, c = lo}

de :: Registers -> U16
de rs = combineBytes rs.d rs.e

setDE :: U16 -> Registers  -> Registers
setDE n rs =
    let (hi, lo) = splitIntoBytes n
    in rs{d = hi, e = lo}

hl :: Registers -> U16
hl rs = combineBytes rs.h rs.l

setHL :: U16 -> Registers  -> Registers
setHL n rs =
    let (hi, lo) = splitIntoBytes n
    in rs{h = hi, l = lo}

af :: Registers -> U16
af rs = combineBytes rs.a rs.f

setAF :: U16 -> Registers  -> Registers
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

type InMemoryScreen = Vector (Vector U8)

emptyScreen :: InMemoryScreen
emptyScreen = Vector.replicate 144 emptyLine
  where
    emptyLine = Vector.replicate 160 0

data CPUState = CPUState
    { _registers :: Registers
    , _memoryBus :: MemoryBus
    , _dividerCounter :: Int
    , _timerCounter :: Int
    , _masterInterruptEnable :: Bool
    , _scanlineCounter :: Int
    , _screen :: InMemoryScreen
    , _preparedScreen :: InMemoryScreen
    , _halted :: Bool
    }
    deriving stock (Show)

makeLenses ''CPUState

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

modifyBusM :: (MemoryBus -> MemoryBus) -> GameBoy ()
modifyBusM fn = modify' $ \s -> s{_memoryBus = fn s._memoryBus}

modifyRegistersM :: (Registers -> Registers) -> GameBoy ()
modifyRegistersM fn = modify' $ \s -> s{_registers = fn s._registers}

type GameBoy a = StateT CPUState IO a
