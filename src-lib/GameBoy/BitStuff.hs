module GameBoy.BitStuff where

import Data.Bits ((.&.), (.<<.), (.>>.), (.|.))
import Data.Bits qualified as Bits
import Data.Int
import Data.Word
import Numeric qualified
import Optics

type U8 = Word8
type I8 = Int8
type U16 = Word16

toHex :: Integral a => a -> String
toHex n = "$" <> flip Numeric.showHex mempty n

combineBytes :: U8 -> U8 -> U16
combineBytes hi lo = (fromIntegral hi .<<. 8) .|. fromIntegral lo

splitIntoBytes :: U16 -> (U8, U8)
splitIntoBytes n = (fromIntegral (n .>>. 8), fromIntegral (n .&. 0xff))

bit :: Int -> Lens' U8 Bool
bit i =
    lens
        (`Bits.testBit` i)
        ( \n on ->
            (if on then Bits.setBit else Bits.clearBit) n i
        )
