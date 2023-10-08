{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

-- TODO: check whether using StrictData is beneficial once it works

module GameBoy.State where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bits ((.&.))
import Data.Vector (Vector)
import Data.Vector qualified as Vector

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
    deriving stock (Show)

mkInitialState :: Bool -> MemoryBus -> CPUState
mkInitialState startWithBios bus =
    CPUState
        { registers = initialRegisters
        , memoryBus = if startWithBios then loadBios bus else bus
        , dividerCounter = 0
        , timerCounter = 1024
        , masterInterruptEnable = True
        , scanlineCounter = 456
        , screen = emptyScreen
        , preparedScreen = emptyScreen
        , halted = False
        }
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
            , pc = if startWithBios then 0 else 0x100
            , sp = 0xfffe
            }

busM :: GameBoy MemoryBus
busM = gets (.memoryBus)

registersM :: GameBoy Registers
registersM = gets (.registers)

modifyBusM :: (MemoryBus -> MemoryBus) -> GameBoy ()
modifyBusM fn = modify' $ \s -> s{memoryBus = fn s.memoryBus}

modifyRegistersM :: (Registers -> Registers) -> GameBoy ()
modifyRegistersM fn = modify' $ \s -> s{registers = fn s.registers}

modifyScreenM :: (InMemoryScreen -> InMemoryScreen) -> GameBoy ()
modifyScreenM f = modify' $ \s -> s{screen = f s.screen}

type GameBoy a = StateT CPUState (ReaderT Cartridge IO) a

newtype Cycles = Cycles {value :: U32}
    deriving newtype (Num)
