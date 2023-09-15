{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Monad.State.Strict
import Optics
import Test.Tasty
import Test.Tasty.HUnit

import GameBoy.BitStuff
import GameBoy.CPU
import GameBoy.Memory
import GameBoy.PPU
import GameBoy.State

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "CPU"
        [ registerTests
        , stackTests
        , determinePixelColorsTests
        , determineTileAddressTests
        , add_spTests
        , testCase "combineBytes" $ combineBytes 0x9a 0x3f @?= 0x9a3f
        , testCase "splitIntoBytes with low byte only" $ splitIntoBytes 0x001f @?= (0, 0x1f)
        , testCase "splitIntoBytes with high byte only" $ splitIntoBytes 0xe200 @?= (0xe2, 0)
        , testCase "splitIntoBytes" $ splitIntoBytes 0xe207 @?= (0xe2, 0x07)
        , testCase "translateTileColors 0" $ translateTileColors 0b01001110 Color0 @?= 2
        , testCase "translateTileColors 1" $ translateTileColors 0b01001110 Color1 @?= 3
        , testCase "translateTileColors 2" $ translateTileColors 0b01001110 Color2 @?= 0
        , testCase "translateTileColors 3" $ translateTileColors 0b01001110 Color3 @?= 1
        ]

emptyRegisters :: Registers
emptyRegisters = Registers 0 0 0 0 0 0 0 0 0 0

-- TODO: use quickcheck or similar

registerTests :: TestTree
registerTests =
    testGroup
        "Unit tests for the CPU registers"
        [ testCase "Reading from BC" $ do
            let r = emptyRegisters{_b = 0xe, _c = 0x7}
            view bc r @?= 0x0e07
        , testCase "B and C correct after setting BC" $ do
            let r = set bc 0x131a emptyRegisters
            (r._b, r._c) @?= (0x13, 0x1a)
        , testCase "Reading from DE" $ do
            let r = emptyRegisters{_d = 0xe, _e = 0x7}
            view de r @?= 0x0e07
        , testCase "D and E correct after setting BC" $ do
            let r = set de 0x0f1e emptyRegisters
            (r._d, r._e) @?= (0x0f, 0x1e)
        , testCase "Reading from HL" $ do
            let r = emptyRegisters{_h = 0xe, _l = 0x7}
            view hl r @?= 0x0e07
        , testCase "D and E correct after setting HL" $ do
            let r = set hl 0xf3fe emptyRegisters
            (r._h, r._l) @?= (0xf3, 0xfe)
        ]

stackTests :: TestTree
stackTests =
    testGroup
        "Unit tests for interaction with the memory-mapped stack"
        [ testCase "pop after push" $ do
            let
                action = push 0xabcd >> pop
                res = evalState action (mkInitialState defaultMemoryBus)
            toHex res @?= "$abcd"
        , testCase "Multiple pops after multiple pushes" $ do
            let
                action = do
                    push 0xb0a3
                    push 0x112e
                    push 0x0007
                    x1 <- pop
                    x2 <- pop
                    x3 <- pop
                    pure (x1, x2, x3)
                (x, y, z) = evalState action (mkInitialState defaultMemoryBus)
            toHex x @?= "$7"
            toHex y @?= "$112e"
            toHex z @?= "$b0a3"
        ]

determinePixelColorsTests :: TestTree
determinePixelColorsTests =
    testGroup
        "Unit tests for translating two lines of bits into pixel colors"
        [ testCase "all colors can be read from two bytes" $ do
            let
                byte1 = 0b10101110
                byte2 = 0b00110101
                expected = [Color1, Color0, Color3, Color2, Color1, Color3, Color1, Color2]
            determinePixelColors byte1 byte2 @?= expected
        ]

determineTileAddressTests :: TestTree
determineTileAddressTests =
    testGroup
        "determineTileAddress"
        [ testCase "handles 8000 addressing mode correctly" $
            toHex (determineTileAddress 2 Mode8000) @?= "$8020"
        , testCase "handles 8800 addressing mode without overflowing tile identifier" $
            toHex (determineTileAddress 2 Mode8800) @?= "$9020"
        , testCase "handles 8800 addressing mode with overflowing tile identifier" $
            toHex (determineTileAddress 255 Mode8800) @?= "$8ff0"
        ]

add_spTests :: TestTree
add_spTests =
    testGroup
        "add_sp"
        [ testCase "n positive, no overflow" $
            let res = add_spPure 0x5 0x3
            in res @?= (0x8, False, False)
        , testCase "n positive, overflow in lower nibble" $
            let res = add_spPure 0xf 0x1
            in res @?= (0x10, True, False)
        , testCase "n positive, overflow in lower byte" $
            let res = add_spPure 0xf0 0x10
            in res @?= (0x100, False, True)
        , testCase "n positive, overflow in lower byte and nibble" $
            let res = add_spPure 0xff 0x1
            in res @?= (0x100, True, True)
        , testCase "n negative, no overflow" $
            let res = add_spPure 0x1 (-0x3)
            in res @?= (0xfffe, False, False)
        , testCase "n negative, overflow in lower byte and nibble" $
            let res = add_spPure 0x5 (-0x3)
            in res @?= (0x2, True, True)
        ]
