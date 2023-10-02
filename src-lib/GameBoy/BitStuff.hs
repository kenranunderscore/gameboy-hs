module GameBoy.BitStuff where

import Data.Bits ((.&.), (.<<.), (.>>.), (.|.))
import Data.Int
import Data.Word
import Numeric qualified

type U8 = Word8
type U16 = Word16
type U32 = Word32

type I8 = Int8
type I16 = Int16
type I32 = Int32

toU16 :: Integral a => a -> U16
toU16 = fromIntegral

toU8 :: Integral a => a -> U8
toU8 = fromIntegral

toI16 :: Integral a => a -> I16
toI16 = fromIntegral

toHex :: Integral a => a -> String
toHex n = "$" <> flip Numeric.showHex mempty n

combineBytes :: U8 -> U8 -> U16
combineBytes hi lo = (fromIntegral hi .<<. 8) .|. fromIntegral lo

splitIntoBytes :: U16 -> (U8, U8)
splitIntoBytes n = (fromIntegral (n .>>. 8), fromIntegral (n .&. 0xff))
