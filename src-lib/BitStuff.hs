module BitStuff where

import Data.Bits ((.&.), (.<<.), (.>>.), (.|.))
import Data.Int
import Data.Word
import Numeric qualified

type U8 = Word8
type I8 = Int8
type U16 = Word16

toHex :: Integral a => a -> String
toHex n = "$" <> flip Numeric.showHex mempty n

combineBytes :: U8 -> U8 -> U16
combineBytes hi lo = (fromIntegral hi .<<. 8) .|. fromIntegral lo

splitIntoBytes :: U16 -> (U8, U8)
splitIntoBytes n = (fromIntegral (n .>>. 8), fromIntegral (n .&. 0xff))
