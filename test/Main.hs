{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

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
        , testCase "translateTileColors 0" $ translateColor 0b01001110 (Color 0) @?= 2
        , testCase "translateTileColors 1" $ translateColor 0b01001110 (Color 1) @?= 3
        , testCase "translateTileColors 2" $ translateColor 0b01001110 (Color 2) @?= 0
        , testCase "translateTileColors 3" $ translateColor 0b01001110 (Color 3) @?= 1
        ]

emptyRegisters :: Registers
emptyRegisters = Registers 0 0 0 0 0 0 0 0 0 0

-- TODO: use quickcheck or similar

registerTests :: TestTree
registerTests =
    testGroup
        "Unit tests for the CPU registers"
        [ testCase "Reading from BC" $ do
            let r = emptyRegisters{b = 0xe, c = 0x7}
            bc r @?= 0x0e07
        , testCase "B and C correct after setting BC" $ do
            let r = setBC 0x131a emptyRegisters
            (r.b, r.c) @?= (0x13, 0x1a)
        , testCase "Reading from DE" $ do
            let r = emptyRegisters{d = 0xe, e = 0x7}
            de r @?= 0x0e07
        , testCase "D and E correct after setting BC" $ do
            let r = setDE 0x0f1e emptyRegisters
            (r.d, r.e) @?= (0x0f, 0x1e)
        , testCase "Reading from HL" $ do
            let r = emptyRegisters{h = 0xe, l = 0x7}
            hl r @?= 0x0e07
        , testCase "D and E correct after setting HL" $ do
            let r = setHL 0xf3fe emptyRegisters
            (r.h, r.l) @?= (0xf3, 0xfe)
        ]

stackTests :: TestTree
stackTests =
    testGroup
        "Unit tests for interaction with the memory-mapped stack"
        [ testCase "pop after push" $ do
            let action = push 0xabcd >> pop
            res <- runGameBoy action defaultCartridge initialState
            toHex res @?= "$abcd"
        , testCase "Multiple pops after multiple pushes" $ do
            let action = do
                    push 0xb0a3
                    push 0x112e
                    push 0x0007
                    x1 <- pop
                    x2 <- pop
                    x3 <- pop
                    pure (x1, x2, x3)
            (x, y, z) <- runGameBoy action defaultCartridge initialState
            toHex x @?= "$7"
            toHex y @?= "$112e"
            toHex z @?= "$b0a3"
        ]
  where
    emptyCartridgeMemory = mkEmptyMemory 0x8000
    defaultBus = mkMemoryBus emptyCartridgeMemory
    defaultCartridge = Cartridge emptyCartridgeMemory Nothing
    initialState = mkInitialState False defaultBus

determinePixelColorsTests :: TestTree
determinePixelColorsTests =
    testGroup
        "Unit tests for translating two lines of bits into pixel colors"
        [ testCase "all colors can be read from two bytes" $ do
            let
                byte1 = 0b10101110
                byte2 = 0b00110101
                expected = [Color 1, Color 0, Color 3, Color 2, Color 1, Color 3, Color 1, Color 2]
            determinePixelColors NoFlip byte1 byte2 @?= expected
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
