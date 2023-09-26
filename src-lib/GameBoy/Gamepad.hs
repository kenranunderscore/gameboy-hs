{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}

module GameBoy.Gamepad where

import Data.Bits ((.|.))
import Data.Bits qualified as Bits
import Data.List (foldl')
import Data.Set (Set)
import Data.Set qualified as Set

import GameBoy.BitStuff

-- TODO: move this stuff into a 'GameBoy' (main) module
data Button = BtnUp | BtnDown | BtnLeft | BtnRight | BtnA | BtnB | BtnSelect | BtnStart
    deriving (Show, Eq, Ord, Enum, Bounded)

buttonBit :: Button -> Int
buttonBit = \case
    BtnDown -> 3
    BtnStart -> 3
    BtnUp -> 2
    BtnSelect -> 2
    BtnLeft -> 1
    BtnB -> 1
    BtnRight -> 0
    BtnA -> 0

type GamepadState = Set Button

noButtonsPressed :: GamepadState
noButtonsPressed = Set.empty

buttonPressed :: Button -> GamepadState -> Bool
buttonPressed = Set.member

actionButtons :: [Button]
actionButtons = [BtnStart, BtnSelect, BtnA, BtnB]

directionButtons :: [Button]
directionButtons = [BtnUp, BtnDown, BtnLeft, BtnRight]

toJoyp :: GamepadState -> U8 -> U8
toJoyp buttons orig =
    if
        | not $ Bits.testBit val 5 -> apply actionButtons
        | not $ Bits.testBit val 4 -> apply directionButtons
        | otherwise -> val
  where
    val = orig .|. 0xcf
    apply =
        foldl'
            (\n btn -> if buttonPressed btn buttons then Bits.clearBit n (buttonBit btn) else n)
            val
