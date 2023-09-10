{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

-- TODO: check whether using StrictData is beneficial once it works

module GameBoy.State where

import Control.Monad.State.Strict
import Data.Vector (Vector)
import Data.Vector qualified as Vector
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

combineRegisters :: Lens' Registers U8 -> Lens' Registers U8 -> Lens' Registers U16
combineRegisters hiL loL =
    lens
        (\r -> combineBytes (view hiL r) (view loL r))
        (\r n -> let (hi, lo) = splitIntoBytes n in r & hiL !~ hi & loL !~ lo)

bc :: Lens' Registers U16
bc = combineRegisters b c

de :: Lens' Registers U16
de = combineRegisters d e

hl :: Lens' Registers U16
hl = combineRegisters h l

af :: Lens' Registers U16
af = combineRegisters a f

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
    }
    deriving stock (Show)

makeLenses ''CPUState

programCounter :: Lens' CPUState U16
programCounter = registers % pc

stackPointer :: Lens' CPUState U16
stackPointer = registers % sp

mkInitialState :: MemoryBus -> CPUState
mkInitialState bus = CPUState initialRegisters bus 0 1024 True 456 emptyScreen
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

type GameBoy m = MonadState CPUState m

-- TODO: use something more performant than 'String'
showScreen :: InMemoryScreen -> String
showScreen = concatMap (\line -> concatMap show line <> "\n")
