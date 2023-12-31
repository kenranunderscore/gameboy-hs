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

import GameBoy.BitStuff
import GameBoy.Cycles
import GameBoy.Memory
import GameBoy.State

data TileMapArea = Area9800 | Area9C00
    deriving (Show)

windowTileMapArea :: MemoryBus -> TileMapArea
windowTileMapArea bus =
    if Bits.testBit (lcdc bus) 6 then Area9C00 else Area9800

bgTileMapArea :: MemoryBus -> TileMapArea
bgTileMapArea bus =
    if Bits.testBit (lcdc bus) 3 then Area9C00 else Area9800

data AddressingMode = Mode8000 | Mode8800
    deriving (Show)

addressingMode :: MemoryBus -> AddressingMode
addressingMode bus =
    if Bits.testBit (lcdc bus) 4 then Mode8000 else Mode8800

newtype Color = Color {unColor :: U8}
    deriving (Eq, Show)

determinePixelColor :: U8 -> U8 -> Int -> Color
determinePixelColor b1 b2 i =
    Color $
        ((b2 .&. Bits.bit i) .>>. i) .<<. 1
            + (b1 .&. Bits.bit i) .>>. i

determinePixelColors :: FlipMode -> U8 -> U8 -> [Color]
determinePixelColors flipMode b1 b2 =
    fmap (determinePixelColor b1 b2) is
  where
    is =
        ( case flipMode of
            FlipX -> id
            FlipBoth -> id
            _ -> reverse
        )
            [0 .. 7]

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

data FlipMode = FlipX | FlipY | FlipBoth | NoFlip
    deriving (Show)

readTileRow :: MemoryBus -> FlipMode -> U16 -> Int -> [Color]
readTileRow bus flipMode addr row =
    determinePixelColors
        flipMode
        (readByte bus (addr + 2 * i))
        (readByte bus (addr + 2 * i + 1))
  where
    i = fromIntegral $
        case flipMode of
            FlipY -> 7 - row
            FlipBoth -> 7 - row
            _ -> row

readPixel :: MemoryBus -> U16 -> U8 -> U8 -> Color
readPixel bus addr row col =
    determinePixelColor
        (readByte bus (addr + 2 * fromIntegral row))
        (readByte bus (addr + 2 * fromIntegral row + 1))
        (fromIntegral $ 7 - col)

data ScanlineColors = ScanlineColors
    { index :: Int
    , -- TODO: find out why using [] here instead of Vector yields 60% fps
      -- _gain_!!
      colors :: [U8]
    }

translateColor :: U8 -> Color -> U8
translateColor palette = \case
    Color 0 -> palette .&. 0b11
    Color 1 -> (palette .>>. 2) .&. 0b11
    Color 2 -> (palette .>>. 4) .&. 0b11
    Color 3 -> (palette .>>. 6) .&. 0b11
    _ -> error "impossible color"

readScanlineColors :: MemoryBus -> ScanlineColors
readScanlineColors bus =
    let
        y = viewportY bus
        x = viewportX bus
        wy = windowY bus
        wx = windowX bus - 7
        mode = addressingMode bus
        currentLine = scanline bus
        useWindow = displayWindow bus && wy <= currentLine
        tileMapStart = determineTileMapAddr useWindow
        ypos = if useWindow then currentLine - wy else currentLine + y
        vertTileIndexOffset = (toU16 ypos .>>. 3) .<<. 5
        currentPalette = bgPalette bus
    in
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
                        colIndex = xpos `mod` 8
                        pixel = readPixel bus tileAddr rowIndex colIndex
                    in
                        translateColor currentPalette pixel
                )
                [0 .. 159]
  where
    determineTileMapAddr useWindow =
        tileMapAreaToAddr $
            if useWindow
                then windowTileMapArea bus
                else bgTileMapArea bus
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

drawScanline :: GameBoy ()
drawScanline = do
    bus <- busM
    let scanlineColors = readScanlineColors bus
    modifyScreenM (Vector.// [(scanlineColors.index, Vector.fromList scanlineColors.colors)])

readFlipMode :: U8 -> FlipMode
readFlipMode spriteAttrs =
    case (Bits.testBit spriteAttrs 5, Bits.testBit spriteAttrs 6) of
        (False, False) -> NoFlip
        (True, False) -> FlipX
        (False, True) -> FlipY
        _ -> FlipBoth

drawSprites :: GameBoy ()
drawSprites = do
    bus <- busM
    -- FIXME: implement object priority here:
    --  1) smaller x coordinate == higher priority
    --  2) equal x => first in OAM wins
    forM_ ([0 .. 39] :: [U16]) $ \sprite -> do
        let
            spriteIndex = sprite * 4 -- 4 bytes per sprite
            y = oamRead spriteIndex bus - 16
            x = fromIntegral $ oamRead (spriteIndex + 1) bus - 8
            tileOffset = oamRead (spriteIndex + 2) bus
            attrs = oamRead (spriteIndex + 3) bus
            flipMode = readFlipMode attrs
            currentLine = fromIntegral $ scanline bus
            height = if spriteUsesTwoTiles bus then 16 else 8
            line = currentLine - y
        when (line >= 0 && line < height) $ do
            let
                tileMemStart = 0x8000 + 16 * fromIntegral tileOffset
                tileRow = readTileRow bus flipMode tileMemStart (fromIntegral line)
                palette = readByte bus $ if Bits.testBit attrs 4 then 0xff49 else 0xff48
            modifyScreenM $ \scr ->
                let
                    v = scr Vector.! fromIntegral currentLine
                    v' =
                        v
                            Vector.// ( filter (\(x', color) -> color /= 0 && x' < 160) $
                                            zipWith
                                                (\color i -> (x + i, translateColor palette color))
                                                tileRow
                                                [0 ..]
                                      )
                in
                    scr Vector.// [(fromIntegral currentLine, v')]

setLcdStatus :: GameBoy ()
setLcdStatus = do
    s <- get
    let status = lcdStatus s.memoryBus
    if lcdEnable s.memoryBus -- TODO: check status instead
        then do
            let
                line = scanline s.memoryBus
                oldMode = status .&. 0b11
                (newMode, needStatInterrupt, newStatus) =
                    determineNextLcdStatus s.scanlineCounter line status
            when (newMode /= oldMode && newMode == 3) $ do
                -- FIXME/TODO: check BG priority here!
                drawScanline
                when (objEnabled s.memoryBus) drawSprites
            when (needStatInterrupt && newMode /= oldMode) $
                modifyBusM $
                    requestInterrupt 1
            compareValue <- lyc <$> busM
            -- coincidence check
            if line == compareValue
                then do
                    let finalStatus = Bits.setBit newStatus 2
                    when (Bits.testBit finalStatus 6) $
                        modifyBusM $
                            requestInterrupt 1
                    modifyBusM $ setSTAT finalStatus
                else modifyBusM $ setSTAT (Bits.clearBit newStatus 2)
        else do
            modify' $ \s' ->
                s'
                    { screen = emptyScreen
                    , preparedScreen = emptyScreen
                    , scanlineCounter = 456
                    }
            modifyBusM $ modifyScanline (const 0)
            -- TODO refactor mode reading/setting
            -- set vblank mode
            let status' = Bits.setBit (Bits.clearBit status 1) 0
            modifyBusM $ setSTAT status'

updateGraphics :: Cycles -> GameBoy ()
updateGraphics cycles = do
    setLcdStatus
    s <- get
    when (lcdEnable s.memoryBus) $ do
        let counter = s.scanlineCounter - fromIntegral cycles.value
        if counter > 0
            then modify' $ \s' -> s'{scanlineCounter = counter}
            else do
                modify' $ \s' -> s'{scanlineCounter = s'.scanlineCounter + 456}
                modifyBusM $ modifyScanline (+ 1)
                line <- scanline <$> busM
                if
                    | line == 144 -> do
                        modify' $ \s' -> s'{screen = emptyScreen, preparedScreen = s'.screen}
                        modifyBusM $ requestInterrupt 0
                    | line > 153 -> do
                        modifyBusM $ modifyScanline (const 0)
                    | otherwise -> pure ()
