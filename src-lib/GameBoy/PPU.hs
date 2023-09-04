{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module GameBoy.PPU where

import Control.Monad
import Control.Monad.State.Strict
import Data.Bits qualified as Bits
import Data.Int (Int32)
import Optics

import Data.Bits ((.&.))
import GameBoy.BitStuff
import GameBoy.CPU
import GameBoy.Memory

data TileMapArea = Area9800 | Area9C00

tileMapAreaIso :: Iso' Bool TileMapArea
tileMapAreaIso =
    iso
        (\x -> if x then Area9C00 else Area9800)
        (\case Area9C00 -> True; Area9800 -> False)

windowTileMapArea :: Lens' MemoryBus TileMapArea
windowTileMapArea = lcdc % bit 6 % tileMapAreaIso

data AddressingMode = Mode8000 | Mode8800

addressingModeIso :: Iso' Bool AddressingMode
addressingModeIso =
    iso
        (\x -> if x then Mode8000 else Mode8800)
        (\case Mode8000 -> True; Mode8800 -> False)

addressingMode :: Lens' MemoryBus AddressingMode
addressingMode = lcdc % bit 4 % addressingModeIso

bgTileMapArea :: Lens' MemoryBus TileMapArea
bgTileMapArea = lcdc % bit 3 % tileMapAreaIso

data Priority = PixelPriority | IgnorePixelPriority

priorityIso :: Iso' Bool Priority
priorityIso =
    iso
        (\x -> if x then IgnorePixelPriority else PixelPriority)
        (\case IgnorePixelPriority -> True; PixelPriority -> False)

bgWindowMasterPriority :: Lens' MemoryBus Priority
bgWindowMasterPriority = lcdc % bit 0 % priorityIso

data ObjectAttributes = ObjectAttributes
    { _y :: U8
    , _x :: U8
    , _tile :: U8
    , _flags :: U8
    }

makeLenses ''ObjectAttributes

updateGraphics :: CPU m => Int -> m ()
updateGraphics cycles = do
    setLcdStatus
    s <- get
    when (s ^. memoryBus % lcdEnable) $ do
        let counter = s ^. scanlineCounter - cycles
        if counter > 0
            then assign' scanlineCounter counter
            else do
                assign' scanlineCounter 456
                modifying' (memoryBus % scanline) (+ 1)
                line <- use (memoryBus % scanline)
                if
                    | line < 144 ->
                        drawScanline
                    | line == 144 ->
                        assign' (memoryBus % interruptFlags % bit 0) True
                    | line > 153 ->
                        assign' (memoryBus % scanline) 0
                    | otherwise -> pure ()
  where
    drawScanline = pure () -- TODO: draw stuff

setLcdStatus :: CPU m => m ()
setLcdStatus = do
    s <- get
    let status = s ^. memoryBus % lcdStatus
    if s ^. memoryBus % lcdEnable
        then do
            let
                line = s ^. memoryBus % scanline
                oldMode = (s ^. memoryBus % lcdStatus) .&. 0b11
                (newMode, needStatInterrupt, newStatus) =
                    determineNextLcdStatus (s ^. scanlineCounter) line status
            when (needStatInterrupt && newMode /= oldMode) $
                -- FIXME: actually set the mode?
                assign' (memoryBus % interruptFlags % bit 1) True
            compareValue <- use (memoryBus % lyc)
            -- coincidence check
            if line == compareValue
                then do
                    let finalStatus = Bits.setBit newStatus 2
                    when (Bits.testBit finalStatus 6) $
                        (assign' (memoryBus % interruptFlags % bit 1) True)
                    assign' (memoryBus % lcdStatus) finalStatus
                else assign' (memoryBus % lcdStatus) (Bits.clearBit newStatus 2)
        else do
            assign' scanlineCounter 456
            assign' (memoryBus % scanline) 0
            -- TODO refactor mode reading/setting
            -- set vblank mode
            let status' = Bits.setBit (Bits.clearBit status 1) 0
            assign' (memoryBus % lcdStatus) status'

determineNextLcdStatus :: Int -> U8 -> U8 -> (U8, Bool, U8)
determineNextLcdStatus counter line status =
    if
        | line >= 144 ->
            ( 1
            , Bits.testBit status 4
            , Bits.setBit (Bits.clearBit status 1) 0
            )
        | counter >= 456 - 80 ->
            ( 2
            , Bits.testBit status 5
            , Bits.setBit (Bits.clearBit status 0) 1
            )
        | counter >= 456 - 80 - 172 ->
            ( 3
            , False
            , Bits.setBit (Bits.setBit status 0) 1
            )
        | otherwise ->
            ( 0
            , Bits.testBit status 3
            , Bits.clearBit (Bits.clearBit status 0) 1
            )

data Color = Color0 | Color1 | Color2 | Color3
    deriving (Eq, Show)

determinePixelColors :: U8 -> U8 -> [Color]
determinePixelColors b1 b2 =
    fmap
        ( \i ->
            case (Bits.testBit b2 i, Bits.testBit b1 i) of
                (False, False) -> Color0
                (False, True) -> Color1
                (True, False) -> Color2
                (True, True) -> Color3
        )
        (reverse [0 .. 7])

determineTileAddress :: U8 -> AddressingMode -> U16
determineTileAddress tileIdentifier = \case
    Mode8000 ->
        0x8000 + 16 * fromIntegral tileIdentifier
    Mode8800 ->
        let
            asI8 :: I8 = fromIntegral tileIdentifier
            asI32 :: Int32 = fromIntegral asI8
        in
            fromIntegral $ (0x9000 :: Int32) + 16 * asI32
