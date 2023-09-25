module GameBoy.Gamepad where

import Data.Set (Set)
import Data.Set qualified as Set

-- TODO: move this stuff into a 'GameBoy' (main) module
data Button = BtnUp | BtnDown | BtnLeft | BtnRight | BtnA | BtnB | BtnSelect | BtnStart
    deriving (Show, Eq, Ord, Enum, Bounded)

type GamepadState = Set Button

noButtonsPressed :: GamepadState
noButtonsPressed = Set.empty

buttonPressed :: Button -> GamepadState -> Bool
buttonPressed = Set.member
