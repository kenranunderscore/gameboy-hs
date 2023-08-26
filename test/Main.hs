{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Monad.State.Strict
import qualified Data.Array as Array
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
        , stackTests
        , testCase "combineU8s" $ combineU8s 0x9a 0x3f @?= 0x9a3f
        , testCase "splitU16 with low byte only" $ splitU16 0x001f @?= (0, 0x1f)
        , testCase "splitU16 with high byte only" $ splitU16 0xe200 @?= (0xe2, 0)
        , testCase "splitU16" $ splitU16 0xe207 @?= (0xe2, 0x07)
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
            getBC r @?= 0x0e07
        , testCase "B and C correct after setting BC" $ do
            let r = setBC emptyRegisters 0x131a
            (r.b, r.c) @?= (0x13, 0x1a)
        , testCase "Reading from DE" $ do
            let r = emptyRegisters{d = 0xe, e = 0x7}
            getDE r @?= 0x0e07
        , testCase "D and E correct after setting BC" $ do
            let r = setDE emptyRegisters 0x0f1e
            (r.d, r.e) @?= (0x0f, 0x1e)
        , testCase "Reading from HL" $ do
            let r = emptyRegisters{h = 0xe, l = 0x7}
            getHL r @?= 0x0e07
        , testCase "D and E correct after setting HL" $ do
            let r = setHL emptyRegisters 0xf3fe
            (r.h, r.l) @?= (0xf3, 0xfe)
        ]

stackTests :: TestTree
stackTests =
    testGroup
        "Unit tests for interaction with the memory-mapped stack"
        [ testCase "pop after push" $ do
            let
                mem = Array.listArray (0, 0xffff) (replicate 0x10000 0)
                action = push 0xabcd >> pop
                res = evalState action (mkInitialState mem)
            toHex res @?= "$abcd"
        , testCase "Multiple pops after multiple pushes" $ do
            let
                mem = Array.listArray (0, 0xffff) (replicate 0x10000 0)
                action = do
                    push 0xb0a3
                    push 0x112e
                    push 0x0007
                    x1 <- pop
                    x2 <- pop
                    x3 <- pop
                    pure (x1, x2, x3)
                (x, y, z) = evalState action (mkInitialState mem)
            toHex x @?= "$7"
            toHex y @?= "$112e"
            toHex z @?= "$b0a3"
        ]
