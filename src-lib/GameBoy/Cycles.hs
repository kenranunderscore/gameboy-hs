{-# LANGUAGE DerivingStrategies #-}

module GameBoy.Cycles (
    Cycles (..),
    lookupCycles,
    lookupCyclesPrefixed,
) where

import Data.Vector.Unboxed qualified as Unboxed

import GameBoy.BitStuff

newtype Cycles = Cycles {value :: U32}
    deriving newtype (Num)

{- FOURMOLU_DISABLE -}
instrCycles :: Unboxed.Vector U32
instrCycles = Unboxed.fromList $!
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

prefixedCycles :: Unboxed.Vector U32
prefixedCycles = Unboxed.fromList $!
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
    , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- a
    , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- b
    , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- c
    , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- d
    , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- e
    , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- f
    ]
   -- 0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
{- FOURMOLU_ENABLE -}

lookupCycles :: U8 -> Cycles
lookupCycles n = Cycles $ 4 * instrCycles Unboxed.! fromIntegral n

lookupCyclesPrefixed :: U8 -> Cycles
lookupCyclesPrefixed n = Cycles $ 4 * prefixedCycles Unboxed.! fromIntegral n
