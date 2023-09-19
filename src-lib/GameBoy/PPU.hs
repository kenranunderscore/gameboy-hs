{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module GameBoy.PPU where

import Control.Monad
import Control.Monad.State.Strict
import Data.Bits ((.&.), (.<<.), (.>>.))
import Data.Bits qualified as Bits
import Data.Int (Int32)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Debug.Trace
import Optics

import GameBoy.BitStuff
import GameBoy.Memory
import GameBoy.State

data TileMapArea = Area9800 | Area9C00
    deriving (Show)

tileMapAreaIso :: Iso' Bool TileMapArea
tileMapAreaIso =
    iso
        (\x -> if x then Area9C00 else Area9800)
        (\case Area9C00 -> True; Area9800 -> False)

windowTileMapArea :: Lens' MemoryBus TileMapArea
windowTileMapArea = lcdc % bit 6 % tileMapAreaIso

data AddressingMode = Mode8000 | Mode8800
    deriving (Show)

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

data Color = Color0 | Color1 | Color2 | Color3
    deriving (Eq, Show, Enum)

determinePixelColors :: U8 -> U8 -> Vector Color
determinePixelColors b1 b2 =
    fmap
        ( \i ->
            case (Bits.testBit b2 i, Bits.testBit b1 i) of
                (False, False) -> Color0
                (False, True) -> Color1
                (True, False) -> Color2
                (True, True) -> Color3
        )
        (Vector.fromList $ reverse [0 .. 7])

determineTileAddress :: U8 -> AddressingMode -> U16
determineTileAddress tileIdentifier = \case
    Mode8000 ->
        0x8000 + 16 * fromIntegral tileIdentifier
    Mode8800 ->
        -- TODO: use bit operations
        let
            asI8 :: I8 = fromIntegral tileIdentifier
            asI32 :: Int32 = fromIntegral asI8
        in
            fromIntegral $ (0x9000 :: Int32) + 16 * asI32

type Tile = Vector (Vector Color)

readTile :: MemoryBus -> U16 -> Tile
readTile bus addr =
    fmap
        ( \i ->
            determinePixelColors
                (readByte bus (addr + 2 * i))
                (readByte bus (addr + 2 * i + 1))
        )
        (Vector.fromList [0 .. 7])

data ScanlineColors = ScanlineColors
    { _index :: Int
    , _colors :: Vector U8
    }

makeLenses ''ScanlineColors

translateTileColors :: U8 -> Color -> U8
translateTileColors palette = \case
    Color0 -> palette .&. 0b11
    Color1 -> (palette .>>. 2) .&. 0b11
    Color2 -> (palette .>>. 4) .&. 0b11
    Color3 -> (palette .>>. 6) .&. 0b11

readScanlineColors :: MemoryBus -> ScanlineColors
readScanlineColors bus =
    let
        y = bus ^. viewportY
        x = bus ^. viewportX
        wy = bus ^. windowY
        wx = bus ^. windowX - 7
        mode = bus ^. addressingMode
        currentLine = bus ^. scanline
        useWindow = view displayWindow bus && wy <= currentLine
        tileMapStart = determineTileMapAddr useWindow
        ypos :: U16 = if useWindow then fromIntegral currentLine - fromIntegral wy else fromIntegral currentLine + fromIntegral y
        vertTileIndexOffset = (ypos .>>. 3) .<<. 5
        currentPalette = bus ^. bgPalette
    in
        -- TODO: "preload" only the necessary tiles, _then_ loop
        ScanlineColors (fromIntegral currentLine) $
            fmap
                ( \i ->
                    let
                        xpos = if useWindow && i >= wx then i - wx else x + i
                        horTileIndex = xpos .>>. 3
                        tileIdentifierAddr = tileMapStart + vertTileIndexOffset + fromIntegral horTileIndex
                        tileIdentifier = readByte bus tileIdentifierAddr
                        tileAddr = determineTileAddress tileIdentifier mode
                        rowIndex = ypos `mod` 8
                        tileColors = readTile bus tileAddr Vector.! fromIntegral rowIndex
                    in
                        translateTileColors currentPalette $!
                            tileColors Vector.! fromIntegral (xpos `mod` 8)
                )
                [0 .. 159]
  where
    determineTileMapAddr useWindow =
        tileMapAreaToAddr $
            if useWindow
                then bus ^. windowTileMapArea
                else bus ^. bgTileMapArea
    tileMapAreaToAddr = \case
        Area9800 -> 0x9800
        Area9C00 -> 0x9C00

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

setLcdStatus :: GameBoy m => m ()
setLcdStatus = do
    s <- get
    let status = s ^. memoryBus % lcdStatus
    if s ^. memoryBus % lcdEnable -- TODO: check status instead
        then do
            let
                line = s ^. memoryBus % scanline
                oldMode = (s ^. memoryBus % lcdStatus) .&. 0b11
                (newMode, needStatInterrupt, newStatus) =
                    determineNextLcdStatus (s ^. scanlineCounter) line status
            when (newMode /= oldMode && newMode == 3) $ do
                bus <- use memoryBus
                let scanlineColors = readScanlineColors bus
                -- traceM $ " MODIFYING SCANLINE: " <> show scanlineColors._index
                traceM $ "      MODE: " <> show (view addressingMode bus)
                traceM $ "      MAP: " <> show (view bgTileMapArea bus)
                modifying'
                    screen
                    ( Vector.//
                        [(scanlineColors._index, scanlineColors._colors)]
                    )
            when (needStatInterrupt && newMode /= oldMode) $
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
            assign' screen emptyScreen
            assign' preparedScreen emptyScreen
            assign' scanlineCounter 456
            assign' (memoryBus % scanline) 0
            -- TODO refactor mode reading/setting
            -- set vblank mode
            let status' = Bits.setBit (Bits.clearBit status 1) 0
            assign' (memoryBus % lcdStatus) status'

updateGraphics :: GameBoy m => Int -> m ()
updateGraphics cycles = do
    setLcdStatus
    s <- get
    when (s ^. memoryBus % lcdEnable) $ do
        let counter = s ^. scanlineCounter - cycles
        if counter > 0
            then assign' scanlineCounter counter
            else do
                modifying' scanlineCounter (+ 456)
                modifying' (memoryBus % scanline) (+ 1)
                line <- use (memoryBus % scanline)
                traceM $ " SCANLINE == " <> show line
                if
                    -- scr <- use screen
                    -- traceShowM scr
                    | line == 144 -> do
                        prepared <- use screen
                        assign' preparedScreen prepared
                        assign' screen emptyScreen
                        assign' (memoryBus % interruptFlags % bit 0) True
                    | line > 153 -> do
                        -- FIXME wrong line constant
                        assign' (memoryBus % scanline) 0
                    | otherwise -> pure ()
