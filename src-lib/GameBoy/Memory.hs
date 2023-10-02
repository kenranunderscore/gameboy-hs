{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module GameBoy.Memory where

import Data.Bits qualified as Bits
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Data.Vector.Unboxed qualified as Unboxed
import Data.Vector.Unboxed.Mutable qualified as MUnboxed

import GameBoy.BitStuff
import GameBoy.Gamepad
import GameBoy.State

-- | Set the n-th byte of a memory array to a fixed value.
setByteAt :: U16 -> Memory -> U8 -> GameBoy ()
setByteAt addr mem val =
    MUnboxed.write mem (fromIntegral addr) val

readByte :: MemoryBus -> U16 -> GameBoy U8
readByte bus addr
    | addr < 0x8000 = MUnboxed.read bus.cartridge $ fromIntegral addr
    | addr < 0xa000 = MUnboxed.read bus.vram $ fromIntegral (addr - 0x8000)
    | addr < 0xc000 = MUnboxed.read bus.sram $ fromIntegral (addr - 0xa000)
    | addr < 0xe000 = MUnboxed.read bus.wram $ fromIntegral (addr - 0xc000)
    | addr < 0xfe00 = MUnboxed.read bus.wram $ fromIntegral (addr - 0xc000) -- echoes WRAM
    | addr < 0xfea0 = oamRead (addr - 0xfe00) bus
    | addr < 0xff00 = pure 0 -- forbidden area
    | addr == 0xff00 = readGamepad bus
    | addr < 0xff80 = MUnboxed.read bus.io $ fromIntegral (addr - 0xff00)
    | addr < 0xffff = MUnboxed.read bus.hram $ fromIntegral (addr - 0xff80)
    | addr == 0xffff = pure bus.ie
    | otherwise = error "the impossible happened"

oamRead :: U16 -> MemoryBus -> GameBoy U8
oamRead relAddr bus = MUnboxed.read bus.oam $ fromIntegral relAddr

readGamepad :: MemoryBus -> GameBoy U8
readGamepad bus = do
    val <- MUnboxed.read bus.io 0
    pure $ toJoyp bus.gamepadState val

readByteM :: U16 -> GameBoy U8
readByteM addr = do
    bus <- busM
    readByte bus addr

-- TODO: refactor! maybe don't take bus?
writeByte :: U16 -> U8 -> MemoryBus -> GameBoy ()
writeByte addr n bus
    -- writes to cartridge ROM cannot happen, but can be used to trigger MBC
    -- interaction
    | addr < 0x8000 = pure ()
    | addr < 0xa000 = writeTo bus.vram 0x8000
    | addr < 0xc000 = writeTo bus.sram 0xa000
    | addr < 0xe000 = writeTo bus.wram 0xc000
    | addr < 0xfe00 = writeTo bus.wram 0xc000 -- echoes WRAM
    | addr < 0xfea0 = writeTo bus.oam 0xfe00
    | addr < 0xff00 = pure () -- forbidden area
    | addr < 0xff80 = writeIO addr n bus
    | addr < 0xffff = writeTo bus.hram 0xff80
    | addr == 0xffff = modifyBusM $ const bus{ie = n}
    | otherwise = error "the impossible happened"
  where
    writeTo dest offset =
        setByteAt (addr - offset) dest n

writeIO :: U16 -> U8 -> MemoryBus -> GameBoy ()
writeIO addr n bus =
    case addr of
        -- writing anything to the divider or scanline register resets them
        0xff04 -> setByteAt offset bus.io 0
        0xff44 -> setByteAt offset bus.io 0
        _ -> setByteAt offset bus.io n
  where
    offset = addr - 0xff00

readU16 :: MemoryBus -> U16 -> GameBoy U16
readU16 bus addr =
    -- GameBoy is little-endian
    combineBytes
        <$> readByte bus (addr + 1)
        <*> readByte bus addr

scanline :: MemoryBus -> GameBoy U8
scanline bus = readByte bus 0xff44

lcdc :: MemoryBus -> GameBoy U8
lcdc bus = readByte bus 0xff40

lcdEnable :: MemoryBus -> GameBoy Bool
lcdEnable bus = (`Bits.testBit` 7) <$> lcdc bus

displayWindow :: MemoryBus -> GameBoy Bool
displayWindow bus = (`Bits.testBit` 5) <$> lcdc bus

spriteUsesTwoTiles :: MemoryBus -> GameBoy Bool
spriteUsesTwoTiles bus = (`Bits.testBit` 2) <$> lcdc bus

objEnabled :: MemoryBus -> GameBoy Bool
objEnabled bus = (`Bits.testBit` 1) <$> lcdc bus

lcdStatus :: MemoryBus -> GameBoy U8
lcdStatus bus = readByte bus 0xff41

setSTATM :: U8 -> GameBoy ()
setSTATM n = do
    bus <- busM
    setByteAt 0x41 bus.io n

viewportY :: MemoryBus -> GameBoy U8
viewportY bus = readByte bus 0xff42

viewportX :: MemoryBus -> GameBoy U8
viewportX bus = readByte bus 0xff43

lyc :: MemoryBus -> GameBoy U8
lyc bus = readByte bus 0xff45

windowY :: MemoryBus -> GameBoy U8
windowY bus = readByte bus 0xff4a

windowX :: MemoryBus -> GameBoy U8
windowX bus = readByte bus 0x4ffb

modifyDivider :: (U8 -> U8) -> MemoryBus -> GameBoy ()
modifyDivider f bus = do
    orig <- readByte bus 0xff04
    setByteAt 0x04 bus.io (f orig)

tima :: MemoryBus -> GameBoy U8
tima bus = readByte bus 0xff05

modifyTimaM :: (U8 -> U8) -> GameBoy ()
modifyTimaM f = do
    bus <- busM
    orig <- readByte bus 0xff05
    setByteAt 0x05 bus.io (f orig)

tma :: MemoryBus -> GameBoy U8
tma bus = readByte bus 0xff06

tac :: MemoryBus -> GameBoy U8
tac bus = readByte bus 0xff07

bgPalette :: MemoryBus -> GameBoy U8
bgPalette bus = readByte bus 0xff47

timerEnable :: MemoryBus -> GameBoy Bool
timerEnable bus = (`Bits.testBit` 2) <$> tac bus

interruptFlags :: MemoryBus -> GameBoy U8
interruptFlags bus = readByte bus 0xff0f

interruptRequested :: Int -> MemoryBus -> GameBoy Bool
interruptRequested interrupt bus =
    (`Bits.testBit` interrupt) <$> interruptFlags bus

timerIntRequested :: MemoryBus -> GameBoy Bool
timerIntRequested = interruptRequested 2

toggleInterrupt :: Bool -> Int -> MemoryBus -> GameBoy ()
toggleInterrupt enabled interrupt bus = do
    let change = if enabled then Bits.setBit else Bits.clearBit
    val <- (`change` interrupt) <$> readByte bus 0xff0f
    writeByte 0xff0f val bus

requestInterrupt :: Int -> MemoryBus -> GameBoy ()
requestInterrupt = toggleInterrupt True

requestInterruptM :: Int -> GameBoy ()
requestInterruptM interrupt =
    toggleInterrupt True interrupt =<< busM

disableInterrupt :: Int -> MemoryBus -> GameBoy ()
disableInterrupt = toggleInterrupt False

{- FOURMOLU_DISABLE -}

bios :: CartridgeMemory
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

ioInitialValues :: CartridgeMemory
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

type CartridgeMemory = Unboxed.Vector U8

data Cartridge = Cartridge
    { memory :: CartridgeMemory
    , header :: Maybe CartridgeHeader
    }
    deriving (Show)

readCartridgeHeader :: CartridgeMemory -> Maybe CartridgeHeader
readCartridgeHeader mem =
    Just $
        CartridgeHeader
            theTitle
            (mem Unboxed.! 0x143)
            (mem Unboxed.! 0x146)
            (readCartridgeType $ mem Unboxed.! 0x147)
  where
    theTitle =
        Unboxed.toList $
            Unboxed.map (Char.chr . fromIntegral) $
                Unboxed.slice 0x134 (0x142 - 0x134 + 1) mem

loadCartridgeFromFile :: FilePath -> IO Cartridge
loadCartridgeFromFile path = do
    bytes <- BS.readFile path
    let mem = Unboxed.fromList $ BS.unpack bytes
    pure $ Cartridge mem (readCartridgeHeader mem)

mkDefaultMemoryBus :: CartridgeMemory -> IO MemoryBus
mkDefaultMemoryBus cart = do
    cartridge <- Unboxed.thaw cart
    vram <- mkEmptyMemory 0x2000
    sram <- mkEmptyMemory 0x2000
    wram <- mkEmptyMemory 0x2000
    oam <- mkEmptyMemory 0x100
    hram <- mkEmptyMemory 0x80
    io <- Unboxed.thaw ioInitialValues
    -- TODO: set correct initial values
    pure
        MemoryBus
            { cartridge = cartridge
            , vram = vram
            , sram = sram
            , wram = wram
            , oam = oam
            , gamepadState = noButtonsPressed
            , io = io
            , hram = hram
            , ie = 0x0 -- TODO check this
            }

mkEmptyMemory :: U16 -> IO Memory
mkEmptyMemory len =
    let v = Unboxed.replicate (fromIntegral len) 0
    in Unboxed.thaw v

initializeMemoryBus :: FilePath -> IO MemoryBus
initializeMemoryBus path = do
    cart <- loadCartridgeFromFile path
    case cart.header of
        Nothing -> putStrLn "No cartridge header could be read"
        Just h -> do
            putStrLn $ "Loaded cartride:  " <> h.title
            putStrLn $ "Cartridge type:  " <> show h.cartridgeType
            putStrLn $ "CGB:  " <> toHex h.cgb
            putStrLn $ "SGB:  " <> toHex h.sgb
    mkDefaultMemoryBus cart.memory
