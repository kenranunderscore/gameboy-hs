{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Emulator

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Emulator"
        [ registerTests
        , testCase "combineU8s" $ combineU8s 0x9a 0x3f @?= 0x9a3f
        , testCase "splitU16 with low byte only" $ splitU16 0x001f @?= (0, 0x1f)
        , testCase "splitU16 with high byte only" $ splitU16 0xe200 @?= (0xe2, 0)
        , testCase "splitU16" $ splitU16 0xe207 @?= (0xe2, 0x07)
        ]

registerTests :: TestTree
registerTests =
    testGroup
        "Unit tests for the CPU registers"
        [ testCase "Reading from BC" $ do
            let r = Registers 0 0 0xe 0x7 0 0 0 0 0 0
            getBC r @?= 0x0e07
        , testCase "B and C correct after setting BC" $ do
            let r = setBC initialRegisters 0x131a
            (r.b, r.c) @?= (0x13, 0x1a)
        , testCase "Reading from DE" $ do
            let r = Registers 0 0 0 0 0xe 0x7 0 0 0 0
            getDE r @?= 0x0e07
        , testCase "D and E correct after setting BC" $ do
            let r = setDE initialRegisters 0x0f1e
            (r.d, r.e) @?= (0x0f, 0x1e)
        , testCase "Reading from HL" $ do
            let r = Registers 0 0 0 0 0 0 0xe 0x7 0 0
            getHL r @?= 0x0e07
        , testCase "D and E correct after setting HL" $ do
            let r = setHL initialRegisters 0xf3fe
            (r.h, r.l) @?= (0xf3, 0xfe)
        ]
