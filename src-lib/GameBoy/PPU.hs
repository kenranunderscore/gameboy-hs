{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module GameBoy.PPU where

import Control.Monad
import Control.Monad.Extra (whenM)
import Control.Monad.State.Strict
import Data.Bits ((.&.), (.<<.), (.>>.))
import Data.Bits qualified as Bits
import Data.Int (Int32)
import Data.Vector (Vector)
import Data.Vector qualified as Vector

import GameBoy.BitStuff
import GameBoy.Memory
import GameBoy.State

data TileMapArea = Area9800 | Area9C00
    deriving (Show)

windowTileMapArea :: MemoryBus -> GameBoy TileMapArea
windowTileMapArea bus = do
    lcdc' <- lcdc bus
    pure $ if Bits.testBit lcdc' 6 then Area9C00 else Area9800

bgTileMapArea :: MemoryBus -> GameBoy TileMapArea
bgTileMapArea bus = do
    lcdc' <- lcdc bus
    pure $ if Bits.testBit lcdc' 3 then Area9C00 else Area9800

data AddressingMode = Mode8000 | Mode8800
    deriving (Show)

addressingMode :: MemoryBus -> GameBoy AddressingMode
addressingMode bus = do
    lcdc' <- lcdc bus
    pure $ if Bits.testBit lcdc' 4 then Mode8000 else Mode8800

data Color = Color0 | Color1 | Color2 | Color3
    deriving (Eq, Show, Enum)

determinePixelColors :: FlipMode -> U8 -> U8 -> Vector Color
determinePixelColors flipMode b1 b2 =
    fmap
        ( \i ->
            case (Bits.testBit b2 i, Bits.testBit b1 i) of
                (False, False) -> Color0
                (False, True) -> Color1
                (True, False) -> Color2
                (True, True) -> Color3
        )
        (Vector.fromList is)
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

readTileRow :: MemoryBus -> FlipMode -> U16 -> Int -> GameBoy (Vector Color)
readTileRow bus flipMode addr row =
    determinePixelColors
        flipMode
        <$> readByte bus (addr + 2 * i)
        <*> readByte bus (addr + 2 * i + 1)
  where
    i = fromIntegral $
        case flipMode of
            FlipY -> 7 - row
            FlipBoth -> 7 - row
            _ -> row

data ScanlineColors = ScanlineColors
    { index :: Int
    , colors :: Vector U8
    }

translateTileColors :: U8 -> Color -> U8
translateTileColors palette = \case
    Color0 -> palette .&. 0b11
    Color1 -> (palette .>>. 2) .&. 0b11
    Color2 -> (palette .>>. 4) .&. 0b11
    Color3 -> (palette .>>. 6) .&. 0b11

readScanlineColors :: MemoryBus -> GameBoy ScanlineColors
readScanlineColors bus = do
    y <- viewportY bus
    x <- viewportX bus
    wy <- windowY bus
    wx' <- windowX bus
    let wx = wx' - 7
    mode <- addressingMode bus
    currentLine <- scanline bus
    useWindow <- (&& wy <= currentLine) <$> displayWindow bus
    tileMapStart <- determineTileMapAddr useWindow
    currentPalette <- bgPalette bus
    let
        ypos = if useWindow then currentLine - wy else currentLine + y
        vertTileIndexOffset = (toU16 ypos .>>. 3) .<<. 5
    -- TODO: "preload" only the necessary tiles, _then_ loop
    ScanlineColors (fromIntegral currentLine)
        <$> traverse
            ( \i -> do
                let
                    xpos = if useWindow && i >= wx then i - wx else x + i
                    horTileIndex = xpos .>>. 3
                    tileIdentifierAddr = tileMapStart + vertTileIndexOffset + fromIntegral horTileIndex
                tileIdentifier <- readByte bus tileIdentifierAddr
                let
                    tileAddr = determineTileAddress tileIdentifier mode
                    rowIndex = ypos `mod` 8
                tileColors <- readTileRow bus NoFlip tileAddr (fromIntegral rowIndex)
                pure $
                    translateTileColors currentPalette $
                        tileColors Vector.! fromIntegral (xpos `mod` 8)
            )
            [0 .. 159]
  where
    determineTileMapAddr useWindow =
        tileMapAreaToAddr
            <$> if useWindow
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
    scanlineColors <- readScanlineColors bus
    modifyScreenM (Vector.// [(scanlineColors.index, scanlineColors.colors)])

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
    forM_ ([0 .. 39] :: [U16]) $ \sprite -> do
        let spriteIndex = sprite * 4 -- 4 bytes per sprite
        y <- (\x -> x - 16) <$> oamRead spriteIndex bus
        x <- (\x -> x - 8) <$> oamRead (spriteIndex + 1) bus
        tileOffset <- oamRead (spriteIndex + 2) bus
        attrs <- oamRead (spriteIndex + 3) bus
        let flipMode = readFlipMode attrs
        currentLine <- fromIntegral <$> scanline bus
        useTwoTiles <- spriteUsesTwoTiles bus
        let
            height = if useTwoTiles then 16 else 8
            line = currentLine - y
        when (line >= 0 && line < height) $ do
            -- FIXME: transparency!
            let tileMemStart = 0x8000 + 16 * fromIntegral tileOffset
            tileRow <- readTileRow bus flipMode tileMemStart (fromIntegral line)
            palette <- readByte bus $ if Bits.testBit attrs 4 then 0xff49 else 0xff48
            modifyScreenM $ \scr ->
                let
                    v = scr Vector.! fromIntegral currentLine
                    v' =
                        v
                            Vector.// ( fmap
                                            ( \i ->
                                                ( fromIntegral x + i
                                                , fmap (translateTileColors palette) tileRow Vector.! i
                                                )
                                            )
                                            [0 .. 7]
                                      )
                in
                    scr Vector.// [(fromIntegral currentLine, v')]

setLcdStatus :: GameBoy ()
setLcdStatus = do
    s <- get
    status <- lcdStatus s.memoryBus
    lcdEnabled <- lcdEnable s.memoryBus
    if lcdEnabled -- TODO: check status instead
        then do
            line <- scanline s.memoryBus
            let
                oldMode = status .&. 0b11
                (newMode, needStatInterrupt, newStatus) =
                    determineNextLcdStatus s.scanlineCounter line status
            when (newMode /= oldMode && newMode == 3) $ do
                -- FIXME/TODO: check BG priority here!
                drawScanline
                whenM (objEnabled s.memoryBus) drawSprites
            when (needStatInterrupt && newMode /= oldMode) $
                requestInterruptM 1
            compareValue <- lyc =<< busM
            -- coincidence check
            if line == compareValue
                then do
                    let finalStatus = Bits.setBit newStatus 2
                    when (Bits.testBit finalStatus 6) $
                        requestInterruptM 1
                    setSTATM finalStatus
                else setSTATM (Bits.clearBit newStatus 2)
        else do
            modify' $ \s' ->
                s'
                    { screen = emptyScreen
                    , preparedScreen = emptyScreen
                    , scanlineCounter = 456
                    }
            bus <- busM
            setByteAt 0x44 bus.io 0 -- TODO: DIV
            -- TODO refactor mode reading/setting
            -- set vblank mode
            let status' = Bits.setBit (Bits.clearBit status 1) 0
            setSTATM status'

updateGraphics :: Int -> GameBoy ()
updateGraphics cycles = do
    setLcdStatus
    s <- get
    whenM (lcdEnable s.memoryBus) $ do
        let counter = s.scanlineCounter - cycles
        if counter > 0
            then modify' $ \s' -> s'{scanlineCounter = counter}
            else do
                modify' $ \s' -> s'{scanlineCounter = s'.scanlineCounter + 456}
                bus <- busM
                div' <- readByte bus 0xff44
                setByteAt 0x44 bus.io (div' + 1) -- TODO: DIV
                line <- scanline bus
                if
                    | line == 144 -> do
                        modify' $ \s' -> s'{screen = emptyScreen, preparedScreen = s'.screen}
                        requestInterruptM 0
                    | line > 153 -> do
                        setByteAt 0x44 bus.io 0
                    | otherwise -> pure ()
