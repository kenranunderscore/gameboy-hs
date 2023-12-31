{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module GameBoy.Memory (
    Cartridge (..),
    MemoryBus (..),
    MemoryWriteEffect (..),
    TimerFrequency (..),
    bgPalette,
    disableInterrupt,
    displayWindow,
    getMemoryBank,
    interruptFlags,
    lcdEnable,
    lcdStatus,
    lcdc,
    loadBios,
    loadCartridgeFromFile,
    lyc,
    mkEmptyMemory,
    mkMemoryBus,
    modifyDivider,
    modifyScanline,
    modifyTima,
    oamRead,
    objEnabled,
    readByte,
    readU16,
    requestInterrupt,
    scanline,
    setSTAT,
    spriteUsesTwoTiles,
    tac,
    tima,
    timerEnable,
    timerFrequency,
    tma,
    viewportX,
    viewportY,
    windowX,
    windowY,
    writeByte,
)
where

import Data.Bits qualified as Bits
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Data.Vector.Unboxed (Vector, (!), (//))
import Data.Vector.Unboxed qualified as Unboxed

import GameBoy.BitStuff
import GameBoy.Gamepad

type Memory = Vector U8

data MemoryBus = MemoryBus
    { cartridge :: Memory
    -- ^ Cartridge ROM: $0000 - $3fff
    , romBank :: Memory
    -- ^ Interchangeable ROM bank: $4000 - $7fff
    , vram :: Memory
    -- ^ VRAM: $8000 - $9fff
    , sram :: Memory
    -- ^ SRAM: $a000 - $bfff
    , wram :: Memory
    -- ^ WRAM: $c000 - $dfff
    , oam :: Memory
    -- ^ Object attribute memory: $fe00 - $fe9f
    , gamepadState :: GamepadState
    -- ^ Current state of the buttons: $ff00
    , io :: Memory
    -- ^ IO registers: $ff00 - $ff7f
    , hram :: Memory
    -- ^ HRAM: $ff80 - $fffe
    , ie :: U8
    -- ^ Interrupt register: $ffff
    }
    deriving (Show)

loadBios :: MemoryBus -> MemoryBus
loadBios bus =
    bus{cartridge = Unboxed.update bus.cartridge (Unboxed.indexed bios)}

-- | Set the n-th byte of a memory array to a fixed value.
setByteAt :: U16 -> Memory -> U8 -> Memory
setByteAt addr mem val = mem // [(fromIntegral addr, val)]

readByte :: MemoryBus -> U16 -> U8
readByte bus addr
    | addr < 0x4000 = bus.cartridge ! fromIntegral addr
    | addr < 0x8000 = bus.romBank ! fromIntegral (addr - 0x4000)
    | addr < 0xa000 = bus.vram ! fromIntegral (addr - 0x8000)
    | addr < 0xc000 = bus.sram ! fromIntegral (addr - 0xa000)
    | addr < 0xe000 = bus.wram ! fromIntegral (addr - 0xc000)
    | addr < 0xfe00 = bus.wram ! fromIntegral (addr - 0xc000) -- echoes WRAM
    | addr < 0xfea0 = oamRead (addr - 0xfe00) bus
    | addr < 0xff00 = 0 -- forbidden area
    | addr == 0xff00 = readGamepad bus
    | addr < 0xff80 = bus.io ! fromIntegral (addr - 0xff00)
    | addr < 0xffff = bus.hram ! fromIntegral (addr - 0xff80)
    | addr == 0xffff = bus.ie
    | otherwise = error "the impossible happened"

oamRead :: U16 -> MemoryBus -> U8
oamRead relAddr bus = bus.oam ! fromIntegral relAddr

readGamepad :: MemoryBus -> U8
readGamepad bus = toJoyp buttons val
  where
    val = bus.io ! 0
    buttons = bus.gamepadState

data MemoryWriteEffect
    = BeginDMATransfer
    | SwitchMemoryBank
    | UnloadBIOS
    | ChangeFrequency TimerFrequency

writeByte :: U16 -> U8 -> MemoryBus -> (MemoryBus, Maybe MemoryWriteEffect)
writeByte addr n bus
    -- writes to cartridge ROM cannot happen, but can be used to trigger MBC
    -- interaction
    | addr < 0x2000 = only bus
    | addr < 0x4000 = (bus, Just SwitchMemoryBank)
    | addr < 0x8000 = only bus
    | addr < 0xa000 = only bus{vram = writeTo bus.vram 0x8000}
    | addr < 0xc000 = only bus{sram = writeTo bus.sram 0xa000}
    | addr < 0xe000 = only bus{wram = writeTo bus.wram 0xc000}
    | addr < 0xfe00 = only bus{wram = writeTo bus.wram 0xc000} -- echoes WRAM
    | addr < 0xfea0 = only bus{oam = writeTo bus.oam 0xfe00}
    | addr < 0xff00 = only bus -- forbidden area
    | addr < 0xff80 = writeIO addr n bus
    | addr < 0xffff = only bus{hram = writeTo bus.hram 0xff80}
    | addr == 0xffff = only bus{ie = n}
    | otherwise = error "the impossible happened"
  where
    only bus' = (bus', Nothing)
    writeTo dest offset =
        setByteAt (addr - offset) dest n

writeIO :: U16 -> U8 -> MemoryBus -> (MemoryBus, Maybe MemoryWriteEffect)
writeIO addr n bus =
    case addr of
        -- writing anything to the divider or scanline register resets them
        0xff04 -> (bus{io = setByteAt offset bus.io 0}, Nothing)
        0xff07 ->
            let
                oldFreq = timerFrequency bus
                bus' = bus{io = setByteAt offset bus.io n}
            in
                (bus', Just $ ChangeFrequency oldFreq)
        0xff44 -> (bus{io = setByteAt offset bus.io 0}, Nothing)
        0xff46 -> (bus, Just BeginDMATransfer)
        _ -> (bus{io = setByteAt offset bus.io n}, Nothing)
  where
    offset = addr - 0xff00

readU16 :: MemoryBus -> U16 -> U16
readU16 bus addr =
    -- GameBoy is little-endian
    combineBytes
        (readByte bus $ addr + 1)
        (readByte bus addr)

scanline :: MemoryBus -> U8
scanline bus = readByte bus 0xff44

modifyScanline :: (U8 -> U8) -> MemoryBus -> MemoryBus
modifyScanline f bus =
    let orig = readByte bus 0xff44
    in bus{io = setByteAt 0x44 bus.io (f orig)}

lcdc :: MemoryBus -> U8
lcdc bus = readByte bus 0xff40

lcdEnable :: MemoryBus -> Bool
lcdEnable bus = Bits.testBit (lcdc bus) 7

displayWindow :: MemoryBus -> Bool
displayWindow bus = Bits.testBit (lcdc bus) 5

spriteUsesTwoTiles :: MemoryBus -> Bool
spriteUsesTwoTiles bus = Bits.testBit (lcdc bus) 2

objEnabled :: MemoryBus -> Bool
objEnabled bus = Bits.testBit (lcdc bus) 1

lcdStatus :: MemoryBus -> U8
lcdStatus bus = readByte bus 0xff41

setSTAT :: U8 -> MemoryBus -> MemoryBus
setSTAT n bus = bus{io = setByteAt 0x41 bus.io n}

viewportY :: MemoryBus -> U8
viewportY bus = readByte bus 0xff42

viewportX :: MemoryBus -> U8
viewportX bus = readByte bus 0xff43

lyc :: MemoryBus -> U8
lyc bus = readByte bus 0xff45

windowY :: MemoryBus -> U8
windowY bus = readByte bus 0xff4a

windowX :: MemoryBus -> U8
windowX bus = readByte bus 0x4ffb

modifyDivider :: (U8 -> U8) -> MemoryBus -> MemoryBus
modifyDivider f bus =
    let orig = readByte bus 0xff04
    in bus{io = setByteAt 0x04 bus.io (f orig)}

tima :: MemoryBus -> U8
tima bus = readByte bus 0xff05

modifyTima :: (U8 -> U8) -> MemoryBus -> MemoryBus
modifyTima f bus =
    let orig = readByte bus 0xff05
    in bus{io = setByteAt 0x05 bus.io (f orig)}

tma :: MemoryBus -> U8
tma bus = readByte bus 0xff06

tac :: MemoryBus -> U8
tac bus = readByte bus 0xff07

data TimerFrequency
    = Freq4K
    | Freq16K
    | Freq64K
    | Freq256K
    deriving (Eq, Show)

readTimerFrequency :: U8 -> TimerFrequency
readTimerFrequency n =
    case (Bits.testBit n 1, Bits.testBit n 0) of
        (False, False) -> Freq4K
        (False, True) -> Freq256K
        (True, False) -> Freq64K
        (True, True) -> Freq16K

timerFrequency :: MemoryBus -> TimerFrequency
timerFrequency = readTimerFrequency . tac

bgPalette :: MemoryBus -> U8
bgPalette bus = readByte bus 0xff47

timerEnable :: MemoryBus -> Bool
timerEnable bus = Bits.testBit (tac bus) 2

interruptFlags :: MemoryBus -> U8
interruptFlags bus = readByte bus 0xff0f

toggleInterrupt :: Bool -> Int -> MemoryBus -> MemoryBus
toggleInterrupt enabled interrupt bus =
    let
        change = if enabled then Bits.setBit else Bits.clearBit
        val = change (readByte bus 0xff0f) interrupt
    in
        fst $ writeByte 0xff0f val bus

requestInterrupt :: Int -> MemoryBus -> MemoryBus
requestInterrupt = toggleInterrupt True

disableInterrupt :: Int -> MemoryBus -> MemoryBus
disableInterrupt = toggleInterrupt False

{- FOURMOLU_DISABLE -}

bios :: Memory
bios =
    [ 0x31, 0xfe, 0xff, 0xaf, 0x21, 0xff, 0x9f, 0x32, 0xcb, 0x7c, 0x20, 0xfb, 0x21, 0x26, 0xff, 0x0e
    , 0x11, 0x3e, 0x80, 0x32, 0xe2, 0x0c, 0x3e, 0xf3, 0xe2, 0x32, 0x3e, 0x77, 0x77, 0x3e, 0xfc, 0xe0
    , 0x47, 0x11, 0x04, 0x01, 0x21, 0x10, 0x80, 0x1a, 0xcd, 0x95, 0x00, 0xcd, 0x96, 0x00, 0x13, 0x7b
    , 0xfe, 0x34, 0x20, 0xf3, 0x11, 0xd8, 0x00, 0x06, 0x08, 0x1a, 0x13, 0x22, 0x23, 0x05, 0x20, 0xf9
    , 0x3e, 0x19, 0xea, 0x10, 0x99, 0x21, 0x2f, 0x99, 0x0e, 0x0c, 0x3d, 0x28, 0x08, 0x32, 0x0d, 0x20
    , 0xf9, 0x2e, 0x0f, 0x18, 0xf3, 0x67, 0x3e, 0x64, 0x57, 0xe0, 0x42, 0x3e, 0x91, 0xe0, 0x40, 0x04
    , 0x1e, 0x02, 0x0e, 0x0c, 0xf0, 0x44, 0xfe, 0x90, 0x20, 0xfa, 0x0d, 0x20, 0xf7, 0x1d, 0x20, 0xf2
    , 0x0e, 0x13, 0x24, 0x7c, 0x1e, 0x83, 0xfe, 0x62, 0x28, 0x06, 0x1e, 0xc1, 0xfe, 0x64, 0x20, 0x06
    , 0x7b, 0xe2, 0x0c, 0x3e, 0x87, 0xe2, 0xf0, 0x42, 0x90, 0xe0, 0x42, 0x15, 0x20, 0xd2, 0x05, 0x20
    , 0x4f, 0x16, 0x20, 0x18, 0xcb, 0x4f, 0x06, 0x04, 0xc5, 0xcb, 0x11, 0x17, 0xc1, 0xcb, 0x11, 0x17
    , 0x05, 0x20, 0xf5, 0x22, 0x23, 0x22, 0x23, 0xc9, 0xce, 0xed, 0x66, 0x66, 0xcc, 0x0d, 0x00, 0x0b
    , 0x03, 0x73, 0x00, 0x83, 0x00, 0x0c, 0x00, 0x0d, 0x00, 0x08, 0x11, 0x1f, 0x88, 0x89, 0x00, 0x0e
    , 0xdc, 0xcc, 0x6e, 0xe6, 0xdd, 0xdd, 0xd9, 0x99, 0xbb, 0xbb, 0x67, 0x63, 0x6e, 0x0e, 0xec, 0xcc
    , 0xdd, 0xdc, 0x99, 0x9f, 0xbb, 0xb9, 0x33, 0x3e, 0x3c, 0x42, 0xb9, 0xa5, 0xb9, 0xa5, 0x42, 0x3c
    , 0x21, 0x04, 0x01, 0x11, 0xa8, 0x00, 0x1a, 0x13, 0xbe, 0x20, 0xfe, 0x23, 0x7d, 0xfe, 0x34, 0x20
    , 0xf5, 0x06, 0x19, 0x78, 0x86, 0x23, 0x05, 0x20, 0xfb, 0x86, 0x20, 0xfe, 0x3e, 0x01, 0xe0, 0x50
    ]

ioInitialValues :: Memory
ioInitialValues =
    [ 0xff, 0x00, 0x7c, 0xff, 0x00, 0x00, 0x00, 0xf8, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01
    , 0x80, 0xbf, 0xf3, 0xff, 0xbf, 0xff, 0x3f, 0x00, 0xff, 0xbf, 0x7f, 0xff, 0x9f, 0xff, 0xbf, 0xff
    , 0xff, 0x00, 0x00, 0xbf, 0x77, 0xf3, 0xf1, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
    , 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff
    , 0x91, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfc, 0x00, 0x00, 0x00, 0x00, 0xff, 0x7e, 0xff, 0xfe
    , 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x3e, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
    , 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xc0, 0xff, 0xc1, 0x00, 0xfe, 0xff, 0xff, 0xff
    , 0xf8, 0xff, 0x00, 0x00, 0x00, 0x8f, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
    , 0xce, 0xed, 0x66, 0x66, 0xcc, 0x0d, 0x00, 0x0b, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0c, 0x00, 0x0d
    , 0x00, 0x08, 0x11, 0x1f, 0x88, 0x89, 0x00, 0x0e, 0xdc, 0xcc, 0x6e, 0xe6, 0xdd, 0xdd, 0xd9, 0x99
    , 0xbb, 0xbb, 0x67, 0x63, 0x6e, 0x0e, 0xec, 0xcc, 0xdd, 0xdc, 0x99, 0x9f, 0xbb, 0xb9, 0x33, 0x3e
    , 0x45, 0xec, 0x52, 0xfa, 0x08, 0xb7, 0x07, 0x5d, 0x01, 0xfd, 0xc0, 0xff, 0x08, 0xfc, 0x00, 0xe5
    , 0x0b, 0xf8, 0xc2, 0xce, 0xf4, 0xf9, 0x0f, 0x7f, 0x45, 0x6d, 0x3d, 0xfe, 0x46, 0x97, 0x33, 0x5e
    , 0x08, 0xef, 0xf1, 0xff, 0x86, 0x83, 0x24, 0x74, 0x12, 0xfc, 0x00, 0x9f, 0xb4, 0xb7, 0x06, 0xd5
    , 0xd0, 0x7a, 0x00, 0x9e, 0x04, 0x5f, 0x41, 0x2f, 0x1d, 0x77, 0x36, 0x75, 0x81, 0xaa, 0x70, 0x3a
    , 0x98, 0xd1, 0x71, 0x02, 0x4d, 0x01, 0xc1, 0xff, 0x0d, 0x00, 0xd3, 0x05, 0xf9, 0x00, 0x0b, 0x0
    ]

{- FOURMOLU_ENABLE -}

data CartridgeType
    = NoMBC
    | MBC1
    | MBC1_RAM
    | MBC1_RAM_Battery
    | UnhandledCartridgeType U8
    deriving (Show)

readCartridgeType :: U8 -> CartridgeType
readCartridgeType = \case
    0 -> NoMBC
    1 -> MBC1
    2 -> MBC1_RAM
    3 -> MBC1_RAM_Battery
    unhandled -> UnhandledCartridgeType unhandled

data CartridgeHeader = CartridgeHeader
    { title :: String
    , cgb :: U8
    , sgb :: U8
    , cartridgeType :: CartridgeType
    }
    deriving (Show)

data Cartridge = Cartridge
    { memory :: Memory
    , header :: Maybe CartridgeHeader
    }
    deriving (Show)

-- FIXME: incomplete
getMemoryBank :: U8 -> Memory -> Memory
getMemoryBank n cart =
    Unboxed.slice (fromIntegral n * 0x4000) 0x4000 cart

readCartridgeHeader :: Memory -> Maybe CartridgeHeader
readCartridgeHeader mem =
    Just $
        CartridgeHeader
            theTitle
            (mem ! 0x143)
            (mem ! 0x146)
            (readCartridgeType $ mem ! 0x147)
  where
    theTitle =
        Unboxed.toList $
            Unboxed.map (Char.chr . fromIntegral) $
                Unboxed.slice 0x134 (0x142 - 0x134 + 1) mem

loadCartridgeFromFile :: FilePath -> IO Cartridge
loadCartridgeFromFile path = do
    bytes <- BS.readFile path
    let
        mem = Unboxed.fromList $ BS.unpack bytes
        cart = Cartridge mem (readCartridgeHeader mem)
    case cart.header of
        Nothing -> putStrLn "No cartridge header could be read"
        Just h -> do
            putStrLn $ "Loaded cartride:  " <> h.title
            putStrLn $ "Cartridge type:  " <> show h.cartridgeType
            putStrLn $ "CGB:  " <> toHex h.cgb
            putStrLn $ "SGB:  " <> toHex h.sgb
    pure cart

mkMemoryBus :: Memory -> MemoryBus
mkMemoryBus cart =
    MemoryBus
        { cartridge = getMemoryBank 0 cart
        , romBank = getMemoryBank 1 cart
        , vram = mkEmptyMemory 0x2000
        , sram = mkEmptyMemory 0x2000
        , wram = mkEmptyMemory 0x2000
        , oam = mkEmptyMemory 0x100
        , gamepadState = noButtonsPressed
        , io = ioInitialValues
        , hram = mkEmptyMemory 0x80
        , ie = 0 -- TODO check this
        }

mkEmptyMemory :: U16 -> Memory
mkEmptyMemory len =
    Unboxed.replicate (fromIntegral len) 0
