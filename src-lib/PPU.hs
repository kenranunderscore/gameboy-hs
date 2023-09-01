{-# LANGUAGE LambdaCase #-}

module PPU where

import Optics

import BitStuff
import Memory

data TileMapArea = Area9800 | Area9C00

lcdc :: Lens' MemoryBus U8
lcdc = io % byte 0x40

lcdEnable :: Lens' MemoryBus Bool
lcdEnable = lcdc % bit 7

tileMapAreaIso :: Iso' Bool TileMapArea
tileMapAreaIso =
    iso
        (\x -> if x then Area9C00 else Area9800)
        (\case Area9C00 -> True; Area9800 -> False)

windowTileMapArea :: Lens' MemoryBus TileMapArea
windowTileMapArea = lcdc % bit 6 % tileMapAreaIso

displayWindow :: Lens' MemoryBus Bool
displayWindow = lcdc % bit 5

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

objSize :: Lens' MemoryBus Bool
objSize = lcdc % bit 2

objEnabled :: Lens' MemoryBus Bool
objEnabled = lcdc % bit 1

data Priority = PixelPriority | IgnorePixelPriority

priorityIso :: Iso' Bool Priority
priorityIso =
    iso
        (\x -> if x then IgnorePixelPriority else PixelPriority)
        (\case IgnorePixelPriority -> True; PixelPriority -> False)

bgWindowMasterPriority :: Lens' MemoryBus Priority
bgWindowMasterPriority = lcdc % bit 0 % priorityIso

viewportY :: Lens' MemoryBus U8
viewportY = io % byte 0x42

viewportX :: Lens' MemoryBus U8
viewportX = io % byte 0x43

ly :: Lens' MemoryBus U8
ly = io % byte 0x44

lyc :: Lens' MemoryBus U8
lyc = io % byte 0x45

windowY :: Lens' MemoryBus U8
windowY = io % byte 0x4a

windowX :: Lens' MemoryBus U8
windowX = io % byte 0x4b
