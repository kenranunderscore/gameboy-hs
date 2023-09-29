{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module GameBoy.Main (main) where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as Exception
import Control.Monad
import Control.Monad.State.Strict
import Data.Bits ((.&.), (.<<.), (.>>.), (.|.))
import Data.Bits qualified as Bits
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Data.Foldable (traverse_)
import Data.IORef
import Data.Int
import Data.List (foldl')
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time qualified as Time
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word
import Numeric qualified
import Optics
import SDL qualified
import System.Environment qualified as Environment
import System.IO

type U8 = Word8
type U16 = Word16
type U32 = Word32

type I8 = Int8
type I16 = Int16
type I32 = Int32

toU16 :: Integral a => a -> U16
toU16 = fromIntegral

toU8 :: Integral a => a -> U8
toU8 = fromIntegral

toI16 :: Integral a => a -> I16
toI16 = fromIntegral

toHex :: Integral a => a -> String
toHex n = "$" <> flip Numeric.showHex mempty n

combineBytes :: U8 -> U8 -> U16
combineBytes hi lo = (fromIntegral hi .<<. 8) .|. fromIntegral lo

splitIntoBytes :: U16 -> (U8, U8)
splitIntoBytes n = (fromIntegral (n .>>. 8), fromIntegral (n .&. 0xff))

bit :: Int -> Lens' U8 Bool
bit i =
    lens
        (`Bits.testBit` i)
        ( \n on ->
            (if on then Bits.setBit else Bits.clearBit) n i
        )

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

maxCyclesPerFrame :: Int
maxCyclesPerFrame = 4_194_304 `div` 60

type InMemoryScreen = Vector (Vector U8)

type Memory = Vector U8

data MemoryBus = MemoryBus
    { _cartridge :: Memory
    -- ^ Cartridge RAM: $0000 - $7fff
    , _vram :: Memory
    -- ^ VRAM: $8000 - $9fff
    , _sram :: Memory
    -- ^ SRAM: $a000 - $bfff
    , _wram :: Memory
    -- ^ WRAM: $c000 - $dfff
    , _oam :: Memory
    -- ^ Object attribute memory: $fe00 - $fe9f
    , _gamepadState :: GamepadState
    -- ^ Current state of the buttons: $ff00
    , _io :: Memory
    -- ^ IO registers: $ff00 - $ff7f
    , _hram :: Memory
    -- ^ HRAM: $ff80 - $fffe
    , _ie :: U8
    -- ^ Interrupt register: $ffff
    }
    deriving (Show)

makeLenses ''MemoryBus

data Registers = Registers
    { _a :: U8
    , _b :: U8
    , _c :: U8
    , _d :: U8
    , _e :: U8
    , _h :: U8
    , _l :: U8
    , -- TODO: benchmark later whether a simple Haskell value can be used here
      -- to make everything more readable
      _f :: U8
    , _pc :: U16
    , _sp :: U16
    }
    deriving stock (Eq)

makeLenses ''Registers

combineRegisters :: Lens' Registers U8 -> Lens' Registers U8 -> Lens' Registers U16
combineRegisters hiL loL =
    lens
        (\r -> combineBytes (view hiL r) (view loL r))
        (\r n -> let (hi, lo) = splitIntoBytes n in r & hiL !~ hi & loL !~ lo)

bc :: Lens' Registers U16
bc = combineRegisters b c

de :: Lens' Registers U16
de = combineRegisters d e

hl :: Lens' Registers U16
hl = combineRegisters h l

af :: Lens' Registers U16
af = combineRegisters a f

{- FOURMOLU_DISABLE -}
instance Show Registers where
    show r = mconcat
        [ "AF = " , toHex (view af r)
        , " | BC = " , toHex (view bc r)
        , " | DE = " , toHex (view de r)
        , " | HL = " , toHex (view hl r)
        , " | PC = " , toHex (view pc r)
        , " | SP = " , toHex (view sp r)
        ]
{- FOURMOLU_ENABLE -}

emptyScreen :: InMemoryScreen
emptyScreen = Vector.replicate 144 emptyLine
  where
    emptyLine = Vector.replicate 160 0

data CPUState = CPUState
    { _registers :: Registers
    , _memoryBus :: MemoryBus
    , _dividerCounter :: Int
    , _timerCounter :: Int
    , _masterInterruptEnable :: Bool
    , _scanlineCounter :: Int
    , _screen :: InMemoryScreen
    , _preparedScreen :: InMemoryScreen
    , _halted :: Bool
    }
    deriving stock (Show)

makeLenses ''CPUState

programCounter :: Lens' CPUState U16
programCounter = registers % pc

stackPointer :: Lens' CPUState U16
stackPointer = registers % sp

mkInitialState :: MemoryBus -> CPUState
mkInitialState bus =
    CPUState initialRegisters bus 0 1024 True 456 emptyScreen emptyScreen False
  where
    initialRegisters =
        Registers
            { _a = 0x1
            , _b = 0
            , _c = 0x13
            , _d = 0
            , _e = 0xd8
            , _h = 0x1
            , _l = 0x4d
            , _f = 0xb0
            , _pc = 0x100 -- start without BIOS for now
            , _sp = 0xfffe
            }

type GameBoy a = StateT CPUState IO a

-- | Target the n-th byte of a memory array.  This is (of course) only a lens if
-- and only if the index is inside the boundaries of the array.
byte :: U16 -> Lens' Memory U8
byte n =
    lens
        (\mem -> mem Vector.! fromIntegral n)
        (\mem x -> mem Vector.// [(fromIntegral n, x)])

readByte :: MemoryBus -> U16 -> U8
readByte bus addr
    | addr < 0x8000 = bus._cartridge Vector.! fromIntegral addr
    | addr < 0xa000 = bus._vram Vector.! fromIntegral (addr - 0x8000)
    | addr < 0xc000 = bus._sram Vector.! fromIntegral (addr - 0xa000)
    | addr < 0xe000 = bus._wram Vector.! fromIntegral (addr - 0xc000)
    | addr < 0xfe00 = bus._wram Vector.! fromIntegral (addr - 0xc000) -- echoes WRAM
    | addr < 0xfea0 = bus._oam Vector.! fromIntegral (addr - 0xfe00)
    | addr < 0xff00 = 0 -- forbidden area
    | addr == 0xff00 = readGamepad bus
    | addr < 0xff80 = bus._io Vector.! fromIntegral (addr - 0xff00)
    | addr < 0xffff = bus._hram Vector.! fromIntegral (addr - 0xff80)
    | addr == 0xffff = bus._ie
    | otherwise = error "the impossible happened"

readGamepad :: MemoryBus -> U8
readGamepad bus = toJoyp buttons val
  where
    val = bus._io Vector.! 0
    buttons = bus._gamepadState

writeByte :: U16 -> U8 -> MemoryBus -> MemoryBus
writeByte addr n bus
    -- writes to cartridge ROM cannot happen, but can be used to trigger MBC
    -- interaction
    | addr < 0x8000 = bus
    | addr < 0xa000 = writeTo vram 0x8000
    | addr < 0xc000 = writeTo sram 0xa000
    | addr < 0xe000 = writeTo wram 0xc000
    | addr < 0xfe00 = writeTo wram 0xc000 -- echoes WRAM
    | addr < 0xfea0 = writeTo oam 0xfe00
    | addr < 0xff00 = bus -- forbidden area
    | addr < 0xff80 = writeIO addr n bus
    | addr < 0xffff = writeTo hram 0xff80
    | addr == 0xffff = bus & ie !~ n
    | otherwise = error "the impossible happened"
  where
    writeTo dest offset =
        bus & dest % byte (addr - offset) !~ n

writeIO :: U16 -> U8 -> MemoryBus -> MemoryBus
writeIO addr n bus =
    case addr of
        -- writing anything to the divider or scanline register resets them
        0xff04 -> bus & divider !~ 0
        0xff44 -> bus & scanline !~ 0
        _ -> bus & io % byte relativeAddr !~ n
  where
    relativeAddr = addr - 0xff00

readU16 :: MemoryBus -> U16 -> U16
readU16 bus addr =
    -- GameBoy is little-endian
    combineBytes
        (readByte bus $ addr + 1)
        (readByte bus addr)

scanline :: Lens' MemoryBus U8
scanline = io % byte 0x44

lcdc :: Lens' MemoryBus U8
lcdc = io % byte 0x40

lcdEnable :: Lens' MemoryBus Bool
lcdEnable = lcdc % bit 7

displayWindow :: Lens' MemoryBus Bool
displayWindow = lcdc % bit 5

spriteUsesTwoTiles :: Lens' MemoryBus Bool
spriteUsesTwoTiles = lcdc % bit 2

objEnabled :: Lens' MemoryBus Bool
objEnabled = lcdc % bit 1

lcdStatus :: Lens' MemoryBus U8
lcdStatus = io % byte 0x41

viewportY :: Lens' MemoryBus U8
viewportY = io % byte 0x42

viewportX :: Lens' MemoryBus U8
viewportX = io % byte 0x43

lyc :: Lens' MemoryBus U8
lyc = io % byte 0x45

windowY :: Lens' MemoryBus U8
windowY = io % byte 0x4a

windowX :: Lens' MemoryBus U8
windowX = io % byte 0x4b

divider :: Lens' MemoryBus U8
divider = io % byte 0x04

tima :: Lens' MemoryBus U8
tima = io % byte 0x05

tma :: Lens' MemoryBus U8
tma = io % byte 0x06

tac :: Lens' MemoryBus U8
tac = io % byte 0x07

bgPalette :: Lens' MemoryBus U8
bgPalette = io % byte 0x47

timerEnable :: Lens' MemoryBus Bool
timerEnable = tac % bit 2

interruptFlags :: Lens' MemoryBus U8
interruptFlags = io % byte 0x0f

timerIntRequested :: Lens' MemoryBus Bool
timerIntRequested = interruptFlags % bit 2

dma :: Lens' MemoryBus U8
dma = io % byte 0x46

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
    { _title :: String
    , _cgb :: U8
    , _sgb :: U8
    , _cartridgeType :: CartridgeType
    }
    deriving (Show)

data Cartridge = Cartridge
    { _memory :: Memory
    , _header :: Maybe CartridgeHeader
    }
    deriving (Show)

readCartridgeHeader :: Memory -> Maybe CartridgeHeader
readCartridgeHeader mem =
    Just $
        CartridgeHeader
            theTitle
            (mem Vector.! 0x143)
            (mem Vector.! 0x146)
            (readCartridgeType $ mem Vector.! 0x147)
  where
    theTitle =
        Vector.toList $
            Vector.map (Char.chr . fromIntegral) $
                Vector.slice 0x134 (0x142 - 0x134 + 1) mem

loadCartridgeFromFile :: FilePath -> IO Cartridge
loadCartridgeFromFile path = do
    bytes <- BS.readFile path
    let mem = Vector.fromList $ BS.unpack bytes
    pure $ Cartridge mem (readCartridgeHeader mem)

defaultMemoryBus :: MemoryBus
defaultMemoryBus =
    -- TODO: set correct initial values
    MemoryBus
        { _ie = 0x0 -- TODO check this
        , _hram = mkEmptyMemory 0x80
        , _gamepadState = noButtonsPressed
        , _io = ioInitialValues
        , _oam = mkEmptyMemory 0x100
        , _wram = mkEmptyMemory 0x2000
        , _sram = mkEmptyMemory 0x2000
        , _vram = mkEmptyMemory 0x2000
        , _cartridge = mkEmptyMemory 0x8000
        }

mkEmptyMemory :: U16 -> Memory
mkEmptyMemory len =
    Vector.replicate (fromIntegral len) 0

initializeMemoryBus :: FilePath -> IO MemoryBus
initializeMemoryBus path = do
    cart <- loadCartridgeFromFile path
    case cart._header of
        Nothing -> putStrLn "No cartridge header could be read"
        Just h -> do
            putStrLn $ "Loaded cartride:  " <> h._title
            putStrLn $ "Cartridge type:  " <> show h._cartridgeType
            putStrLn $ "CGB:  " <> toHex h._cgb
            putStrLn $ "SGB:  " <> toHex h._sgb
    pure $ set cartridge cart._memory defaultMemoryBus

-- | Flags, living in the higher nibble of the F register.
data Flag = Zero | Negative | HalfCarry | Carry
    deriving (Show)

flagBit :: Flag -> Int
flagBit = \case
    Zero -> 7
    Negative -> 6
    HalfCarry -> 5
    Carry -> 4

flag :: Flag -> Lens' Registers Bool
flag fl = f % bit (flagBit fl)

clearFlag :: Flag -> Registers -> Registers
clearFlag fl = set (flag fl) False

setFlag :: Flag -> Registers -> Registers
setFlag fl = set (flag fl) True

hasFlag :: Flag -> CPUState -> Bool
hasFlag fl = view (registers % flag fl)

advance :: U16 -> GameBoy ()
advance n = modifying' programCounter (+ n)

data TargetRegister = A | B | C | D | E | H | L
    deriving stock (Show)

targetL :: TargetRegister -> Lens' Registers U8
targetL = \case
    A -> a
    B -> b
    C -> c
    D -> d
    E -> e
    H -> h
    L -> l

data TargetRegister16 = BC | DE | HL
    deriving stock (Show)

target16L :: TargetRegister16 -> Lens' Registers U16
target16L = \case
    BC -> bc
    DE -> de
    HL -> hl

data FlagCondition = ZUnset | ZSet | CUnset | CSet

instance Show FlagCondition where
    show = \case
        ZUnset -> "NZ"
        ZSet -> "Z"
        CUnset -> "NC"
        CSet -> "C"

checkFlagCondition :: FlagCondition -> CPUState -> Bool
checkFlagCondition cond s = check s
  where
    check = case cond of
        ZSet -> hasFlag Zero
        ZUnset -> not . hasFlag Zero
        CSet -> hasFlag Carry
        CUnset -> not . hasFlag Carry

data RestartAddr
    = Rst00
    | Rst08
    | Rst10
    | Rst18
    | Rst20
    | Rst28
    | Rst30
    | Rst38

getRestartAddr :: RestartAddr -> U16
getRestartAddr = \case
    Rst00 -> 0x00
    Rst08 -> 0x08
    Rst10 -> 0x10
    Rst18 -> 0x18
    Rst20 -> 0x20
    Rst28 -> 0x28
    Rst30 -> 0x30
    Rst38 -> 0x38

instance Show RestartAddr where
    show = toHex . getRestartAddr

data Instr
    = LD_u16 TargetRegister16 U16
    | LD_SP_u16 U16
    | LD_u16_SP U16
    | LD_SP_HL
    | LD_A_deref TargetRegister16
    | LD_A_FF00plusU8 U8
    | LD_A_FF00plusC
    | LD_A_HLplus
    | LD_A_HLminus
    | LD_A_derefU16 U16
    | LD_u8 TargetRegister U8
    | LD_FF00plusC_A
    | LD_FF00plusU8_A U8
    | LD_u16_A U16
    | LD_deref_rr_A TargetRegister16
    | LD_HLminus_A
    | LD_HLplus_A
    | LD_HLderef_u8 U8
    | LD_r_HLderef TargetRegister
    | LD TargetRegister TargetRegister
    | LD_HL_SP I8
    | LD_derefHL TargetRegister
    | BIT Int TargetRegister
    | BIT_n_derefHL Int
    | JP U16
    | JP_HL
    | JP_cc FlagCondition U16
    | JR_cc FlagCondition I8
    | JR I8
    | INC TargetRegister
    | INC_derefHL
    | INC16 TargetRegister16
    | INC_SP
    | DEC TargetRegister
    | DEC_derefHL
    | DEC16 TargetRegister16
    | DEC_SP
    | CALL U16
    | CALL_cc FlagCondition U16
    | RET
    | RET_cc FlagCondition
    | RETI
    | PUSH TargetRegister16
    | PUSH_AF
    | POP TargetRegister16
    | POP_AF
    | RLA
    | RRA
    | DI
    | EI
    | NOP
    | OR TargetRegister
    | OR_u8 U8
    | OR_A_HL
    | AND TargetRegister
    | AND_u8 U8
    | AND_A_HL
    | SUB TargetRegister
    | SUB_u8 U8
    | SUB_A_HL
    | ADD TargetRegister
    | ADD_u8 U8
    | ADD_A_HL
    | ADC TargetRegister
    | ADC_u8 U8
    | ADC_A_HL
    | SBC TargetRegister
    | SBC_u8 U8
    | SBC_A_HL
    | CP TargetRegister
    | CP_u8 U8
    | CP_A_HL
    | XOR TargetRegister
    | XOR_u8 U8
    | XOR_A_HL
    | RST RestartAddr
    | CPL
    | SWAP TargetRegister
    | SWAP_derefHL
    | ADD_HL TargetRegister16
    | ADD_HL_SP
    | ADD_SP I8
    | RES Int TargetRegister
    | RES_derefHL Int
    | SET Int TargetRegister
    | SET_derefHL Int
    | SRL TargetRegister
    | SRL_derefHL
    | RR TargetRegister
    | RR_derefHL
    | RRC TargetRegister
    | RRC_derefHL
    | RL TargetRegister
    | RL_derefHL
    | RLC TargetRegister
    | RLC_derefHL
    | SLA TargetRegister
    | SLA_derefHL
    | SRA TargetRegister
    | SRA_derefHL
    | DAA
    | SCF
    | CCF
    | RLCA
    | RRCA
    | HALT
    | STOP

instance Show Instr where
    show = \case
        LD_u16 rr n -> "LD " <> show rr <> "," <> toHex n
        LD_SP_u16 n -> "LD SP," <> toHex n
        LD_u16_SP n -> "LD (" <> toHex n <> "),SP"
        LD_SP_HL -> "LD SP,HL"
        LD_deref_rr_A rr -> "LD (" <> show rr <> "),A"
        LD_r_HLderef r -> "LD " <> show r <> ",(HL)"
        LD_HLminus_A -> "LD (HL-),A"
        LD_HLderef_u8 n -> "LD (HL)," <> toHex n
        LD_HLplus_A -> "LD (HL+),A"
        LD_A_HLplus -> "LD A,(HL+)"
        LD_A_HLminus -> "LD A,(HL-)"
        LD_A_deref rr -> "LD A,(" <> show rr <> ")"
        LD_A_FF00plusU8 n -> "LD A,($ff00+" <> toHex n <> ")"
        LD_A_FF00plusC -> "LD A,($ff00+C)"
        LD_A_derefU16 n -> "LD A,(" <> toHex n <> ")"
        LD r r' -> "LD " <> show r <> "," <> show r'
        LD_u8 r n -> "LD " <> show r <> "," <> toHex n
        LD_FF00plusC_A -> "LD ($ff00+C,A)"
        LD_FF00plusU8_A n -> "LD ($ff00+" <> toHex n <> "),A"
        LD_u16_A n -> "LD (" <> toHex n <> "),A"
        LD_HL_SP n -> "LD HL,SP" <> (if n < 0 then mempty else "+") <> show n
        LD_derefHL r -> "LD (HL)," <> show r
        BIT n r -> "BIT " <> show n <> "," <> show r
        BIT_n_derefHL n -> "BIT " <> show n <> ",(HL)"
        JP n -> "JP " <> toHex n
        JP_HL -> "JP HL"
        JP_cc cond n -> "JP " <> show cond <> "," <> toHex n
        JR_cc cond n -> "JR " <> show cond <> "," <> show n
        JR n -> "JR " <> show n
        INC r -> "INC " <> show r
        INC_derefHL -> "INC (HL)"
        INC16 r -> "INC " <> show r
        INC_SP -> "INC SP"
        DEC r -> "DEC " <> show r
        DEC_derefHL -> "DEC (HL)"
        DEC16 r -> "DEC " <> show r
        DEC_SP -> "DEC SP"
        CALL n -> "CALL " <> toHex n
        CALL_cc cond n -> "CALL " <> show cond <> "," <> toHex n
        RET -> "RET"
        RET_cc cond -> "RET " <> show cond
        RETI -> "RETI"
        PUSH rr -> "PUSH " <> show rr
        PUSH_AF -> "PUSH AF"
        POP rr -> "POP " <> show rr
        POP_AF -> "POP AF"
        RLA -> "RLA"
        RRA -> "RRA"
        DI -> "DI"
        EI -> "EI"
        NOP -> "NOP"
        OR r -> "OR A," <> show r
        OR_u8 n -> "OR A," <> toHex n
        OR_A_HL -> "OR A,HL"
        AND r -> "AND A," <> show r
        AND_u8 n -> "AND A," <> toHex n
        AND_A_HL -> "AND A,HL"
        SUB r -> "SUB A," <> show r
        SUB_u8 n -> "SUB A," <> toHex n
        SUB_A_HL -> "SUB A,HL"
        ADD r -> "ADD A," <> show r
        ADD_u8 n -> "ADD A," <> toHex n
        ADD_A_HL -> "ADD A,HL"
        ADC r -> "ADC A," <> show r
        ADC_u8 n -> "ADC A," <> toHex n
        ADC_A_HL -> "ADC A,HL"
        SBC r -> " A," <> show r
        SBC_u8 n -> "SBC A," <> toHex n
        SBC_A_HL -> "SBC A,HL"
        XOR r -> "XOR A," <> show r
        XOR_u8 n -> "XOR A," <> toHex n
        XOR_A_HL -> "XOR A,HL"
        CP r -> "CP A," <> show r
        CP_u8 n -> "CP A," <> toHex n
        CP_A_HL -> "CP A,HL"
        RST addr -> "RST " <> show addr
        CPL -> "CPL"
        SWAP r -> "SWAP " <> show r
        SWAP_derefHL -> "SWAP (HL)"
        ADD_HL rr -> "ADD HL," <> show rr
        ADD_HL_SP -> "ADD HL,SP"
        ADD_SP n -> "ADD SP," <> show n
        RES n r -> "RES " <> show n <> "," <> show r
        RES_derefHL n -> "RES " <> show n <> ",(HL)"
        SET n r -> "SET " <> show n <> "," <> show r
        SET_derefHL n -> "SET " <> show n <> ",(HL)"
        SRL r -> "SRL " <> show r
        SRL_derefHL -> "SRL (HL)"
        RR r -> "RR " <> show r
        RR_derefHL -> "RR (HL)"
        RRC r -> "RRC " <> show r
        RRC_derefHL -> "RRC (HL)"
        RL r -> "RL " <> show r
        RL_derefHL -> "RL (HL)"
        RLC r -> "RLC " <> show r
        RLC_derefHL -> "RLC (HL)"
        SLA r -> "SLA " <> show r
        SLA_derefHL -> "SLA (HL)"
        SRA r -> "SRA " <> show r
        SRA_derefHL -> "SRA (HL)"
        DAA -> "DAA"
        SCF -> "SCF"
        CCF -> "CCF"
        RLCA -> "RLCA"
        RRCA -> "RRCA"
        HALT -> "HALT"
        STOP -> "STOP"

fetchByteM :: GameBoy U8
fetchByteM = do
    s <- get
    advance 1
    pure $ readByte (view memoryBus s) (view programCounter s)

fetchI8M :: GameBoy I8
fetchI8M = do
    fromIntegral <$> fetchByteM

fetchU16M :: GameBoy U16
fetchU16M = do
    s <- get
    advance 2
    pure $ readU16 (view memoryBus s) (view programCounter s)

{- FOURMOLU_DISABLE -}

lookupCycles :: U8 -> Int
lookupCycles n = 4 * cycles Vector.! fromIntegral n
  where
    cycles = Vector.fromList $!
        [ 1, 3, 2, 2, 1, 1, 2, 1, 5, 2, 2, 2, 1, 1, 2, 1 -- 0
        , 0, 3, 2, 2, 1, 1, 2, 1, 3, 2, 2, 2, 1, 1, 2, 1 -- 1
        , 2, 3, 2, 2, 1, 1, 2, 1, 2, 2, 2, 2, 1, 1, 2, 1 -- 2
        , 2, 3, 2, 2, 3, 3, 3, 1, 2, 2, 2, 2, 1, 1, 2, 1 -- 3
        , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1 -- 4
        , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1 -- 5
        , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1 -- 6
        , 2, 2, 2, 2, 2, 2, 0, 2, 1, 1, 1, 1, 1, 1, 2, 1 -- 7
        , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1 -- 8
        , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1 -- 9
        , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1 -- a
        , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1 -- b
        , 2, 3, 3, 4, 3, 4, 2, 4, 2, 4, 3, 0, 3, 6, 2, 4 -- c
        , 2, 3, 3, 0, 3, 4, 2, 4, 2, 4, 3, 0, 3, 0, 2, 4 -- d
        , 3, 3, 2, 0, 0, 4, 2, 4, 4, 1, 4, 0, 0, 0, 2, 4 -- e
        , 3, 3, 2, 1, 0, 4, 2, 4, 3, 2, 4, 1, 0, 0, 2, 4 -- f
        ]
        -- 0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f

lookupCyclesPrefixed :: U8 -> Int
lookupCyclesPrefixed n = 4 * cycles Vector.! fromIntegral n
  where
    cycles = Vector.fromList $!
        [ 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- 0
        , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- 1
        , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- 2
        , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- 3
        , 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2 -- 4
        , 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2 -- 5
        , 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2 -- 6
        , 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2 -- 7
        , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- 8
        , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- 9
        , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- A
        , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- B
        , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- C
        , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- D
        , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- E
        , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2 -- F
        ]
        -- 0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f

{- FOURMOLU_ENABLE -}

data Instruction = Instruction
    { _baseCycles :: Int
    , _tag :: Instr
    }

instance Show Instruction where
    show Instruction{_tag} = show _tag

fetch :: GameBoy Instruction
fetch = do
    s <- get
    let
        counter = view programCounter s
        bus = view memoryBus s
    advance 1
    let n = readByte bus counter
    if n == 0xcb
        then fetchPrefixed
        else do
            let cycles = lookupCycles n
            instr <- case n of
                0 -> pure NOP
                0x01 -> LD_u16 BC <$> fetchU16M
                0x02 -> pure $ LD_deref_rr_A BC
                0x03 -> pure $ INC16 BC
                0x04 -> pure $ INC B
                0x05 -> pure $ DEC B
                0x06 -> LD_u8 B <$> fetchByteM
                0x07 -> pure RLCA
                0x08 -> LD_u16_SP <$> fetchU16M
                0x09 -> pure $ ADD_HL BC
                0x0a -> pure $ LD_A_deref BC
                0x0b -> pure $ DEC16 BC
                0x0c -> pure $ INC C
                0x0d -> pure $ DEC C
                0x0e -> LD_u8 C <$> fetchByteM
                0x0f -> pure RRCA
                0x10 -> advance 1 >> pure STOP
                0x11 -> LD_u16 DE <$> fetchU16M
                0x12 -> pure $ LD_deref_rr_A DE
                0x13 -> pure $ INC16 DE
                0x14 -> pure $ INC D
                0x15 -> pure $ DEC D
                0x16 -> LD_u8 D <$> fetchByteM
                0x17 -> pure RLA
                0x18 -> JR <$> fetchI8M
                0x19 -> pure $ ADD_HL DE
                0x1a -> pure $ LD_A_deref DE
                0x1b -> pure $ DEC16 DE
                0x1c -> pure $ INC E
                0x1d -> pure $ DEC E
                0x1e -> LD_u8 E <$> fetchByteM
                0x1f -> pure RRA
                0x20 -> JR_cc ZUnset <$> fetchI8M
                0x21 -> LD_u16 HL <$> fetchU16M
                0x22 -> pure LD_HLplus_A
                0x23 -> pure $ INC16 HL
                0x24 -> pure $ INC H
                0x25 -> pure $ DEC H
                0x26 -> LD_u8 H <$> fetchByteM
                0x27 -> pure DAA
                0x28 -> JR_cc ZSet <$> fetchI8M
                0x29 -> pure $ ADD_HL HL
                0x2a -> pure LD_A_HLplus
                0x2b -> pure $ DEC16 HL
                0x2c -> pure $ INC L
                0x2d -> pure $ DEC L
                0x2e -> LD_u8 L <$> fetchByteM
                0x2f -> pure CPL
                0x30 -> JR_cc CUnset <$> fetchI8M
                0x31 -> LD_SP_u16 <$> fetchU16M
                0x32 -> pure LD_HLminus_A
                0x33 -> pure $ INC_SP
                0x34 -> pure INC_derefHL
                0x35 -> pure DEC_derefHL
                0x36 -> LD_HLderef_u8 <$> fetchByteM
                0x37 -> pure SCF
                0x38 -> JR_cc CSet <$> fetchI8M
                0x39 -> pure ADD_HL_SP
                0x3a -> pure LD_A_HLminus
                0x3b -> pure DEC_SP
                0x3c -> pure $ INC A
                0x3d -> pure $ DEC A
                0x3e -> LD_u8 A <$> fetchByteM
                0x3f -> pure CCF
                0x40 -> pure $ LD B B
                0x41 -> pure $ LD B C
                0x42 -> pure $ LD B D
                0x43 -> pure $ LD B E
                0x44 -> pure $ LD B H
                0x45 -> pure $ LD B L
                0x46 -> pure $ LD_r_HLderef B
                0x47 -> pure $ LD B A
                0x48 -> pure $ LD C B
                0x49 -> pure $ LD C C
                0x4a -> pure $ LD C D
                0x4b -> pure $ LD C E
                0x4c -> pure $ LD C H
                0x4d -> pure $ LD C L
                0x4e -> pure $ LD_r_HLderef C
                0x4f -> pure $ LD C A
                0x50 -> pure $ LD D B
                0x51 -> pure $ LD D C
                0x52 -> pure $ LD D D
                0x53 -> pure $ LD D E
                0x54 -> pure $ LD D H
                0x55 -> pure $ LD D L
                0x56 -> pure $ LD_r_HLderef D
                0x57 -> pure $ LD D A
                0x58 -> pure $ LD E B
                0x59 -> pure $ LD E C
                0x5a -> pure $ LD E D
                0x5b -> pure $ LD E E
                0x5c -> pure $ LD E H
                0x5d -> pure $ LD E L
                0x5e -> pure $ LD_r_HLderef E
                0x5f -> pure $ LD E A
                0x60 -> pure $ LD H B
                0x61 -> pure $ LD H C
                0x62 -> pure $ LD H D
                0x63 -> pure $ LD H E
                0x64 -> pure $ LD H H
                0x65 -> pure $ LD H L
                0x66 -> pure $ LD_r_HLderef H
                0x67 -> pure $ LD H A
                0x68 -> pure $ LD L B
                0x69 -> pure $ LD L C
                0x6a -> pure $ LD L D
                0x6b -> pure $ LD L E
                0x6c -> pure $ LD L H
                0x6d -> pure $ LD L L
                0x6e -> pure $ LD_r_HLderef L
                0x6f -> pure $ LD L A
                0x70 -> pure $ LD_derefHL B
                0x71 -> pure $ LD_derefHL C
                0x72 -> pure $ LD_derefHL D
                0x73 -> pure $ LD_derefHL E
                0x74 -> pure $ LD_derefHL H
                0x75 -> pure $ LD_derefHL L
                0x76 -> pure HALT
                0x77 -> pure $ LD_derefHL A
                0x78 -> pure $ LD A B
                0x79 -> pure $ LD A C
                0x7a -> pure $ LD A D
                0x7b -> pure $ LD A E
                0x7c -> pure $ LD A H
                0x7d -> pure $ LD A L
                0x7e -> pure $ LD_r_HLderef A
                0x7f -> pure $ LD A A
                0x80 -> pure $ ADD B
                0x81 -> pure $ ADD C
                0x82 -> pure $ ADD D
                0x83 -> pure $ ADD E
                0x84 -> pure $ ADD H
                0x85 -> pure $ ADD L
                0x86 -> pure ADD_A_HL
                0x87 -> pure $ ADD A
                0x88 -> pure $ ADC B
                0x89 -> pure $ ADC C
                0x8a -> pure $ ADC D
                0x8b -> pure $ ADC E
                0x8c -> pure $ ADC H
                0x8d -> pure $ ADC L
                0x8e -> pure ADC_A_HL
                0x8f -> pure $ ADC A
                0x90 -> pure $ SUB B
                0x91 -> pure $ SUB C
                0x92 -> pure $ SUB D
                0x93 -> pure $ SUB E
                0x94 -> pure $ SUB H
                0x95 -> pure $ SUB L
                0x96 -> pure SUB_A_HL
                0x97 -> pure $ SUB A
                0x98 -> pure $ SBC B
                0x99 -> pure $ SBC C
                0x9a -> pure $ SBC D
                0x9b -> pure $ SBC E
                0x9c -> pure $ SBC H
                0x9d -> pure $ SBC L
                0x9e -> pure SBC_A_HL
                0x9f -> pure $ SBC A
                0xa0 -> pure $ AND B
                0xa1 -> pure $ AND C
                0xa2 -> pure $ AND D
                0xa3 -> pure $ AND E
                0xa4 -> pure $ AND H
                0xa5 -> pure $ AND L
                0xa6 -> pure AND_A_HL
                0xa7 -> pure $ AND A
                0xa8 -> pure $ XOR B
                0xa9 -> pure $ XOR C
                0xaa -> pure $ XOR D
                0xab -> pure $ XOR E
                0xac -> pure $ XOR H
                0xad -> pure $ XOR L
                0xae -> pure XOR_A_HL
                0xaf -> pure $ XOR A
                0xb0 -> pure $ OR B
                0xb1 -> pure $ OR C
                0xb2 -> pure $ OR D
                0xb3 -> pure $ OR E
                0xb4 -> pure $ OR H
                0xb5 -> pure $ OR L
                0xb6 -> pure OR_A_HL
                0xb7 -> pure $ OR A
                0xb8 -> pure $ CP B
                0xb9 -> pure $ CP C
                0xba -> pure $ CP D
                0xbb -> pure $ CP E
                0xbc -> pure $ CP H
                0xbd -> pure $ CP L
                0xbe -> pure CP_A_HL
                0xbf -> pure $ CP A
                0xc0 -> pure $ RET_cc ZUnset
                0xc1 -> pure $ POP BC
                0xc2 -> JP_cc ZUnset <$> fetchU16M
                0xc3 -> JP <$> fetchU16M
                0xc4 -> CALL_cc ZUnset <$> fetchU16M
                0xc5 -> pure $ PUSH BC
                0xc6 -> ADD_u8 <$> fetchByteM
                0xc7 -> pure $ RST Rst00
                0xc8 -> pure $ RET_cc ZSet
                0xc9 -> pure RET
                0xca -> JP_cc ZSet <$> fetchU16M
                0xcc -> CALL_cc ZSet <$> fetchU16M
                0xcd -> CALL <$> fetchU16M
                0xce -> ADC_u8 <$> fetchByteM
                0xcf -> pure $ RST Rst08
                0xd0 -> pure $ RET_cc CUnset
                0xd1 -> pure $ POP DE
                0xd2 -> JP_cc CUnset <$> fetchU16M
                0xd4 -> CALL_cc CUnset <$> fetchU16M
                0xd5 -> pure $ PUSH DE
                0xd6 -> SUB_u8 <$> fetchByteM
                0xd7 -> pure $ RST Rst10
                0xd8 -> pure $ RET_cc CSet
                0xd9 -> pure RETI
                0xda -> JP_cc CSet <$> fetchU16M
                0xdc -> CALL_cc CSet <$> fetchU16M
                0xde -> SBC_u8 <$> fetchByteM
                0xdf -> pure $ RST Rst18
                0xe0 -> LD_FF00plusU8_A <$> fetchByteM
                0xe1 -> pure $ POP HL
                0xe2 -> pure LD_FF00plusC_A
                0xe5 -> pure $ PUSH HL
                0xe6 -> AND_u8 <$> fetchByteM
                0xe7 -> pure $ RST Rst20
                0xe8 -> ADD_SP <$> fetchI8M
                0xe9 -> pure JP_HL
                0xea -> LD_u16_A <$> fetchU16M
                0xee -> XOR_u8 <$> fetchByteM
                0xef -> pure $ RST Rst28
                0xf0 -> LD_A_FF00plusU8 <$> fetchByteM
                0xf1 -> pure POP_AF
                0xf2 -> pure LD_A_FF00plusC
                0xf3 -> pure DI
                0xf5 -> pure PUSH_AF
                0xf6 -> OR_u8 <$> fetchByteM
                0xf7 -> pure $ RST Rst30
                0xf8 -> LD_HL_SP <$> fetchI8M
                0xf9 -> pure LD_SP_HL
                0xfa -> LD_A_derefU16 <$> fetchU16M
                0xfb -> pure EI
                0xfe -> CP_u8 <$> fetchByteM
                0xff -> pure $ RST Rst38
                unknown -> error $ "unknown opcode: " <> toHex unknown
            pure $ Instruction cycles instr

fetchPrefixed :: GameBoy Instruction
fetchPrefixed = do
    s <- get
    let
        counter = view programCounter s
        bus = view memoryBus s
    advance 1
    let
        n = readByte bus counter
        cycles = lookupCyclesPrefixed n
    pure $ Instruction cycles $ case n of
        0x00 -> RLC B
        0x01 -> RLC C
        0x02 -> RLC D
        0x03 -> RLC E
        0x04 -> RLC H
        0x05 -> RLC L
        0x06 -> RLC_derefHL
        0x07 -> RLC A
        0x08 -> RRC B
        0x09 -> RRC C
        0x0a -> RRC D
        0x0b -> RRC E
        0x0c -> RRC H
        0x0d -> RRC L
        0x0e -> RRC_derefHL
        0x0f -> RRC A
        0x10 -> RL B
        0x11 -> RL C
        0x12 -> RL D
        0x13 -> RL E
        0x14 -> RL H
        0x15 -> RL L
        0x16 -> RL_derefHL
        0x17 -> RL A
        0x18 -> RR B
        0x19 -> RR C
        0x1a -> RR D
        0x1b -> RR E
        0x1c -> RR H
        0x1d -> RR L
        0x1e -> RR_derefHL
        0x1f -> RR A
        0x20 -> SLA B
        0x21 -> SLA C
        0x22 -> SLA D
        0x23 -> SLA E
        0x24 -> SLA H
        0x25 -> SLA L
        0x26 -> SLA_derefHL
        0x27 -> SLA A
        0x28 -> SRA B
        0x29 -> SRA C
        0x2a -> SRA D
        0x2b -> SRA E
        0x2c -> SRA H
        0x2d -> SRA L
        0x2e -> SRA_derefHL
        0x2f -> SRA A
        0x30 -> SWAP B
        0x31 -> SWAP C
        0x32 -> SWAP D
        0x33 -> SWAP E
        0x34 -> SWAP H
        0x35 -> SWAP L
        0x36 -> SWAP_derefHL
        0x37 -> SWAP A
        0x38 -> SRL B
        0x39 -> SRL C
        0x3a -> SRL D
        0x3b -> SRL E
        0x3c -> SRL H
        0x3d -> SRL L
        0x3e -> SRL_derefHL
        0x3f -> SRL A
        0x40 -> BIT 0 B
        0x41 -> BIT 0 C
        0x42 -> BIT 0 D
        0x43 -> BIT 0 E
        0x44 -> BIT 0 H
        0x45 -> BIT 0 L
        0x46 -> BIT_n_derefHL 0
        0x47 -> BIT 0 A
        0x48 -> BIT 1 B
        0x49 -> BIT 1 C
        0x4a -> BIT 1 D
        0x4b -> BIT 1 E
        0x4c -> BIT 1 H
        0x4d -> BIT 1 L
        0x4e -> BIT_n_derefHL 1
        0x4f -> BIT 1 A
        0x50 -> BIT 2 B
        0x51 -> BIT 2 C
        0x52 -> BIT 2 D
        0x53 -> BIT 2 E
        0x54 -> BIT 2 H
        0x55 -> BIT 2 L
        0x56 -> BIT_n_derefHL 2
        0x57 -> BIT 2 A
        0x58 -> BIT 3 B
        0x59 -> BIT 3 C
        0x5a -> BIT 3 D
        0x5b -> BIT 3 E
        0x5c -> BIT 3 H
        0x5d -> BIT 3 L
        0x5e -> BIT_n_derefHL 3
        0x5f -> BIT 3 A
        0x60 -> BIT 4 B
        0x61 -> BIT 4 C
        0x62 -> BIT 4 D
        0x63 -> BIT 4 E
        0x64 -> BIT 4 H
        0x65 -> BIT 4 L
        0x66 -> BIT_n_derefHL 4
        0x67 -> BIT 4 A
        0x68 -> BIT 5 B
        0x69 -> BIT 5 C
        0x6a -> BIT 5 D
        0x6b -> BIT 5 E
        0x6c -> BIT 5 H
        0x6d -> BIT 5 L
        0x6e -> BIT_n_derefHL 5
        0x6f -> BIT 5 A
        0x70 -> BIT 6 B
        0x71 -> BIT 6 C
        0x72 -> BIT 6 D
        0x73 -> BIT 6 E
        0x74 -> BIT 6 H
        0x75 -> BIT 6 L
        0x76 -> BIT_n_derefHL 6
        0x77 -> BIT 6 A
        0x78 -> BIT 7 B
        0x79 -> BIT 7 C
        0x7a -> BIT 7 D
        0x7b -> BIT 7 E
        0x7c -> BIT 7 H
        0x7d -> BIT 7 L
        0x7e -> BIT_n_derefHL 7
        0x7f -> BIT 7 A
        0x80 -> RES 0 B
        0x81 -> RES 0 C
        0x82 -> RES 0 D
        0x83 -> RES 0 E
        0x84 -> RES 0 H
        0x85 -> RES 0 L
        0x86 -> RES_derefHL 0
        0x87 -> RES 0 A
        0x88 -> RES 1 B
        0x89 -> RES 1 C
        0x8a -> RES 1 D
        0x8b -> RES 1 E
        0x8c -> RES 1 H
        0x8d -> RES 1 L
        0x8e -> RES_derefHL 1
        0x8f -> RES 1 A
        0x90 -> RES 2 B
        0x91 -> RES 2 C
        0x92 -> RES 2 D
        0x93 -> RES 2 E
        0x94 -> RES 2 H
        0x95 -> RES 2 L
        0x96 -> RES_derefHL 2
        0x97 -> RES 2 A
        0x98 -> RES 3 B
        0x99 -> RES 3 C
        0x9a -> RES 3 D
        0x9b -> RES 3 E
        0x9c -> RES 3 H
        0x9d -> RES 3 L
        0x9e -> RES_derefHL 3
        0x9f -> RES 3 A
        0xa0 -> RES 4 B
        0xa1 -> RES 4 C
        0xa2 -> RES 4 D
        0xa3 -> RES 4 E
        0xa4 -> RES 4 H
        0xa5 -> RES 4 L
        0xa6 -> RES_derefHL 4
        0xa7 -> RES 4 A
        0xa8 -> RES 5 B
        0xa9 -> RES 5 C
        0xaa -> RES 5 D
        0xab -> RES 5 E
        0xac -> RES 5 H
        0xad -> RES 5 L
        0xae -> RES_derefHL 5
        0xaf -> RES 5 A
        0xb0 -> RES 6 B
        0xb1 -> RES 6 C
        0xb2 -> RES 6 D
        0xb3 -> RES 6 E
        0xb4 -> RES 6 H
        0xb5 -> RES 6 L
        0xb6 -> RES_derefHL 6
        0xb7 -> RES 6 A
        0xb8 -> RES 7 B
        0xb9 -> RES 7 C
        0xba -> RES 7 D
        0xbb -> RES 7 E
        0xbc -> RES 7 H
        0xbd -> RES 7 L
        0xbe -> RES_derefHL 7
        0xbf -> RES 7 A
        0xc0 -> SET 0 B
        0xc1 -> SET 0 C
        0xc2 -> SET 0 D
        0xc3 -> SET 0 E
        0xc4 -> SET 0 H
        0xc5 -> SET 0 L
        0xc6 -> SET_derefHL 0
        0xc7 -> SET 0 A
        0xc8 -> SET 1 B
        0xc9 -> SET 1 C
        0xca -> SET 1 D
        0xcb -> SET 1 E
        0xcc -> SET 1 H
        0xcd -> SET 1 L
        0xce -> SET_derefHL 1
        0xcf -> SET 1 A
        0xd0 -> SET 2 B
        0xd1 -> SET 2 C
        0xd2 -> SET 2 D
        0xd3 -> SET 2 E
        0xd4 -> SET 2 H
        0xd5 -> SET 2 L
        0xd6 -> SET_derefHL 2
        0xd7 -> SET 2 A
        0xd8 -> SET 3 B
        0xd9 -> SET 3 C
        0xda -> SET 3 D
        0xdb -> SET 3 E
        0xdc -> SET 3 H
        0xdd -> SET 3 L
        0xde -> SET_derefHL 3
        0xdf -> SET 3 A
        0xe0 -> SET 4 B
        0xe1 -> SET 4 C
        0xe2 -> SET 4 D
        0xe3 -> SET 4 E
        0xe4 -> SET 4 H
        0xe5 -> SET 4 L
        0xe6 -> SET_derefHL 4
        0xe7 -> SET 4 A
        0xe8 -> SET 5 B
        0xe9 -> SET 5 C
        0xea -> SET 5 D
        0xeb -> SET 5 E
        0xec -> SET 5 H
        0xed -> SET 5 L
        0xee -> SET_derefHL 5
        0xef -> SET 5 A
        0xf0 -> SET 6 B
        0xf1 -> SET 6 C
        0xf2 -> SET 6 D
        0xf3 -> SET 6 E
        0xf4 -> SET 6 H
        0xf5 -> SET 6 L
        0xf6 -> SET_derefHL 6
        0xf7 -> SET 6 A
        0xf8 -> SET 7 B
        0xf9 -> SET 7 C
        0xfa -> SET 7 D
        0xfb -> SET 7 E
        0xfc -> SET 7 H
        0xfd -> SET 7 L
        0xfe -> SET_derefHL 7
        0xff -> SET 7 A
        unknown -> error $ "unknown prefixed byte: " <> toHex unknown

writeMemory :: U16 -> U8 -> GameBoy ()
writeMemory addr n =
    -- HACK: "listen" for changes that potentially cascade to other state
    -- changes here
    case addr of
        0xff44 -> do
            -- reset scanline if the CPU writes to it
            modifying' memoryBus (writeByte addr 0)
        0xff46 ->
            -- trace ("    [DMA TRANSFER] : " <> toHex n) (dmaTransfer n)
            dmaTransfer n
        -- HACK: "listen" for changes that potentially cascade to other state
        -- changes here
        0xff07 -> do
            freq <- use (memoryBus % timerFrequency)
            modifying' memoryBus (writeByte addr n)
            freq' <- use (memoryBus % timerFrequency)
            when (freq' /= freq) $
                assign' timerCounter (counterFromFrequency freq')
        _ -> modifying' memoryBus (writeByte addr n)

push :: U16 -> GameBoy ()
push n = do
    s <- get
    let curr = s ^. stackPointer
    modifying' stackPointer (\x -> x - 2)
    writeMemory (curr - 1) hi
    writeMemory (curr - 2) lo
  where
    (hi, lo) = splitIntoBytes n

pop :: GameBoy U16
pop = do
    -- TODO: do I have to zero the popped memory location?
    s <- get
    put (s & registers % sp %!~ (+ 2))
    pure $ readU16 (view memoryBus s) (view stackPointer s)

dec :: Lens' Registers U8 -> GameBoy ()
dec reg = do
    modify' $ \s ->
        let
            old = s ^. registers % reg
            result = old - 1
        in
            s
                & registers
                    %!~ ( set (flag Zero) (result == 0)
                            . setFlag Negative
                            . set (flag HalfCarry) (old .&. 0x0f == 0)
                            . set reg result
                        )

inc :: Lens' Registers U8 -> GameBoy ()
inc reg = do
    modify' $ \s ->
        let
            old = s ^. registers % reg
            result = old + 1
        in
            s
                & registers
                    %!~ ( set (flag Zero) (result == 0)
                            . clearFlag Negative
                            . set (flag HalfCarry) (old .&. 0x0f == 0x0f)
                            . set reg result
                        )

ld_r_r :: TargetRegister -> TargetRegister -> GameBoy ()
ld_r_r r r' = modify' $ \s ->
    s & registers % (targetL r) !~ (s ^. registers % targetL r')

deref :: Lens' Registers U16 -> GameBoy U8
deref rr = do
    s <- get
    pure $ readByte (view memoryBus s) (view (registers % rr) s)

execute :: Instruction -> GameBoy Int
execute Instruction{_tag, _baseCycles} =
    case _tag of
        NOP ->
            pure _baseCycles
        LD_u16 rr n ->
            exec $ assign' (registers % target16L rr) n
        LD_deref_rr_A rr -> exec $ do
            rs <- use registers
            writeMemory (rs ^. target16L rr) (rs ^. a)
        LD_SP_u16 n ->
            exec $ assign' (registers % sp) n
        LD_u16_SP n -> exec $ do
            (hi, lo) <- splitIntoBytes <$> use stackPointer
            writeMemory n lo
            writeMemory (n + 1) hi
        LD_r_HLderef r -> exec $ do
            n <- deref hl
            assign' (registers % targetL r) n
        LD_HLminus_A -> exec $ do
            rs <- use registers
            writeMemory (rs ^. hl) (rs ^. a)
            modifying' (registers % hl) (\x -> x - 1)
        LD_HLplus_A -> exec $ do
            rs <- use registers
            writeMemory (rs ^. hl) (rs ^. a)
            modifying' (registers % hl) (+ 1)
        LD_HLderef_u8 n -> exec $ do
            rs <- use registers
            writeMemory (rs ^. hl) n
        LD_A_deref rr -> exec $ do
            n <- deref (target16L rr)
            assign' (registers % a) n
        LD_A_HLplus -> exec $ do
            n <- deref hl
            modifying'
                registers
                ( \rs ->
                    rs
                        & a !~ n
                        & hl %!~ (+ 1)
                )
        LD_A_HLminus -> exec $ do
            n <- deref hl
            modifying'
                registers
                ( \rs ->
                    rs
                        & a !~ n
                        & hl %!~ (\x -> x - 1)
                )
        LD_A_FF00plusU8 n -> exec $ do
            modify' $ \s ->
                s & registers % a !~ readByte (view memoryBus s) (0xff00 + fromIntegral n)
        LD_A_FF00plusC -> exec $ do
            modify' $ \s ->
                let offset = s ^. registers % c
                in s & registers % a !~ readByte (view memoryBus s) (0xff00 + fromIntegral offset)
        LD_A_derefU16 n -> exec $ do
            modify' $ \s ->
                s & registers % a !~ readByte (view memoryBus s) n
        LD_u8 r n ->
            exec $ assign' (registers % targetL r) n
        LD r r' ->
            exec $ ld_r_r r r'
        LD_u16_A n -> exec $ do
            rs <- use registers
            writeMemory n (rs ^. a)
        LD_FF00plusC_A -> exec $ do
            s <- get
            let offset = fromIntegral $ s ^. registers % c
            writeMemory (0xff00 + offset) (s ^. registers % a)
        LD_FF00plusU8_A n -> exec $ do
            s <- get
            writeMemory (0xff00 + fromIntegral n) (s ^. registers % a)
        LD_derefHL r -> exec $ do
            rs <- use registers
            writeMemory (rs ^. hl) (rs ^. targetL r)
        LD_SP_HL -> exec $ do
            modifying' registers $ \rs ->
                rs & set sp (view hl rs)
        LD_HL_SP n -> exec $ do
            modifying' registers $ \rs ->
                let
                    orig = rs ^. sp
                    res' = fromIntegral @_ @I32 orig + fromIntegral n
                    res = fromIntegral res'
                    needsHalfCarry = toU8 orig .&. 0xf + toU8 n .&. 0xf > 0xf
                    needsCarry = orig .&. 0xff + toU16 (toU8 n) > 0xff
                in
                    rs
                        & hl !~ res
                        & clearFlag Zero
                        & clearFlag Negative
                        & set (flag HalfCarry) needsHalfCarry
                        & set (flag Carry) needsCarry
        BIT n r -> exec $ do
            modify' $ \s ->
                let bitIsSet = Bits.testBit (s ^. registers % targetL r) n
                in s
                    & registers
                        %!~ setFlag HalfCarry
                        . clearFlag Negative
                        . set (flag Zero) (not bitIsSet)
        BIT_n_derefHL n -> exec $ do
            val <- deref hl
            let bitIsSet = Bits.testBit val n
            modifying' registers $ \rs ->
                rs
                    & setFlag HalfCarry
                        . clearFlag Negative
                        . set (flag Zero) (not bitIsSet)
        JR n ->
            exec $ modifying' programCounter (+ fromIntegral n)
        JR_cc cond n -> do
            jump <- gets (checkFlagCondition cond)
            if jump
                then do
                    modifying' programCounter (+ fromIntegral n)
                    pure $ _baseCycles + 4
                else pure _baseCycles
        JP_cc cond n -> do
            jump <- gets (checkFlagCondition cond)
            if jump
                then do
                    assign' programCounter n
                    pure $ _baseCycles + 4
                else pure $ _baseCycles
        RET -> exec $ do
            addr <- pop
            assign' programCounter addr
        RET_cc cond -> do
            jump <- gets (checkFlagCondition cond)
            if jump
                then do
                    addr <- pop
                    assign' programCounter addr
                    pure 20
                else pure _baseCycles
        RETI -> exec $ do
            addr <- pop
            assign' programCounter addr
            assign' masterInterruptEnable True
        CALL n -> exec $ do
            counter <- use programCounter
            push counter
            assign' programCounter n
        CALL_cc cond n -> do
            s <- get
            if (checkFlagCondition cond s)
                then do
                    let counter = view programCounter s
                    push counter
                    assign' programCounter n
                    pure 24
                else pure _baseCycles
        JP n ->
            exec $ assign' programCounter n
        JP_HL -> exec $ do
            modifying' registers $ \rs ->
                rs & pc !~ view hl rs
        INC r ->
            exec $ inc (targetL r)
        INC_derefHL -> exec $ do
            s <- get
            let
                addr = s ^. registers % hl
                orig = readByte (view memoryBus s) addr
                val = orig + 1
            writeMemory addr val
            modifying'
                registers
                ( set (flag Zero) (val == 0)
                    . clearFlag Negative
                    . set (flag HalfCarry) (val .&. 0xf == 0)
                )
        INC16 rr ->
            exec $ modifying' (registers % (target16L rr)) (+ 1)
        INC_SP ->
            exec $ modifying' stackPointer (+ 1)
        DEC r ->
            exec $ dec (targetL r)
        DEC16 rr ->
            exec $ modifying' (registers % (target16L rr)) (\n -> n - 1)
        DEC_SP ->
            exec $ modifying' stackPointer (\n -> n - 1)
        DEC_derefHL -> exec $ do
            s <- get
            let
                addr = s ^. registers % hl
                val = readByte (view memoryBus s) addr - 1
            writeMemory addr val
            modifying'
                registers
                ( set (flag Zero) (val == 0)
                    . setFlag Negative
                    . set (flag HalfCarry) ((val + 1) .&. 0x0f == 0)
                )
        PUSH rr -> exec $ do
            n <- use (registers % target16L rr)
            push n
        PUSH_AF -> exec $ do
            n <- use (registers % af)
            push n
        POP rr -> exec $ do
            n <- pop
            assign' (registers % target16L rr) n
        POP_AF -> exec $ do
            n <- pop
            assign' (registers % af) (n .&. 0xfff0)
        RLA -> exec $ do
            modifying' registers $ \rs ->
                let
                    orig = rs ^. a
                    carry = if view (flag Carry) rs then 1 else 0
                    carry' = Bits.testBit orig 7
                    a' = Bits.shiftL orig 1 + carry
                in
                    rs
                        & set (flag Carry) carry'
                            . clearFlag Zero
                            . clearFlag HalfCarry
                            . clearFlag Negative
                            . set a a'
        RRA -> exec $ do
            modifying' registers $ \rs ->
                let
                    orig = rs ^. a
                    carry = if view (flag Carry) rs then 1 else 0
                    carry' = Bits.testBit orig 0
                    a' = Bits.shiftR orig 1 + Bits.shiftL carry 7
                in
                    rs
                        & set (flag Carry) carry'
                            . clearFlag Zero
                            . clearFlag HalfCarry
                            . clearFlag Negative
                            . set a a'
        DI ->
            exec $ assign' masterInterruptEnable False
        EI ->
            exec $ assign' masterInterruptEnable True
        OR r ->
            exec $ or_a r
        OR_u8 n ->
            exec $ or_a_u8 n
        OR_A_HL ->
            exec $ or_a_u8 =<< deref hl
        AND r ->
            exec $ and_a r
        AND_u8 n ->
            exec $ and_a_u8 n
        AND_A_HL ->
            exec $ and_a_u8 =<< deref hl
        ADD r ->
            exec $ add_a r
        ADD_u8 n ->
            exec $ add_a_u8 n
        ADD_A_HL ->
            exec $ add_a_u8 =<< deref hl
        ADD_HL rr ->
            exec $ add_hl (target16L rr)
        ADD_HL_SP ->
            exec $ add_hl sp
        ADD_SP n ->
            exec $ add_sp n
        SUB r ->
            exec $ sub_a r
        SUB_u8 n ->
            exec $ sub_a_u8 n
        SUB_A_HL ->
            exec $ sub_a_u8 =<< deref hl
        ADC r ->
            exec $ adc_a r
        ADC_u8 n ->
            exec $ adc_a_u8 n
        ADC_A_HL ->
            exec $ adc_a_u8 =<< deref hl
        SBC r ->
            exec $ sbc_a r
        SBC_u8 n ->
            exec $ sbc_a_u8 n
        SBC_A_HL ->
            exec $ sbc_a_u8 =<< deref hl
        CP r ->
            exec $ cp_a r
        CP_u8 n ->
            exec $ cp_a_u8 n
        CP_A_HL ->
            exec $ cp_a_u8 =<< deref hl
        XOR r ->
            exec $ xor_a r
        XOR_u8 n ->
            exec $ xor_a_u8 n
        XOR_A_HL ->
            exec $ xor_a_u8 =<< deref hl
        RST addr -> exec $ do
            counter <- use programCounter
            push counter
            assign' programCounter (getRestartAddr addr)
        CPL -> exec $ do
            modifying' registers $ \rs ->
                rs
                    & a %!~ Bits.complement
                    & setFlag Negative
                    & setFlag HalfCarry
        SWAP r -> exec $ do
            modifying' registers $ \rs ->
                let
                    orig = view (targetL r) rs
                    res = Bits.rotate orig 4
                in
                    rs
                        & targetL r !~ res
                        & clearFlag Negative
                        & clearFlag HalfCarry
                        & clearFlag Carry
                        & set (flag Zero) (res == 0)
        SWAP_derefHL -> exec $ do
            s <- get
            let
                rs = s ^. registers
                addr = rs ^. hl
                orig = readByte (view memoryBus s) addr
                res = Bits.rotate orig 4
            writeMemory addr res
            modifying'
                registers
                ( clearFlag Negative
                    . clearFlag HalfCarry
                    . clearFlag Carry
                    . set (flag Zero) (res == 0)
                )
        RES n r ->
            exec $ modifying' (registers % targetL r) (`Bits.clearBit` n)
        RES_derefHL n -> exec $ do
            s <- get
            let
                rs = s ^. registers
                addr = rs ^. hl
                val = readByte (view memoryBus s) addr
                res = Bits.clearBit val n
            writeMemory addr res
        SET n r ->
            exec $ modifying' (registers % targetL r) (`Bits.setBit` n)
        SET_derefHL n -> exec $ do
            s <- get
            let
                rs = s ^. registers
                addr = rs ^. hl
                val = readByte (view memoryBus s) addr
                res = Bits.setBit val n
            writeMemory addr res
        SRL r -> exec $ do
            modifying' registers $ \rs ->
                let
                    orig = view (targetL r) rs
                    res = orig .>>. 1
                in
                    rs
                        & targetL r !~ res
                        & clearFlag Negative
                        & clearFlag HalfCarry
                        & set (flag Carry) (Bits.testBit orig 0)
                        & set (flag Zero) (res == 0)
        SRL_derefHL -> exec $ do
            s <- get
            let
                addr = s ^. registers % hl
                orig = readByte (view memoryBus s) addr
                res = orig .>>. 1
            writeMemory addr res
            modifying' registers $
                clearFlag Negative
                    . clearFlag HalfCarry
                    . set (flag Carry) (Bits.testBit orig 0)
                    . set (flag Zero) (res == 0)
        RR r -> exec $ do
            modifying' registers $ \rs ->
                let
                    orig = rs ^. targetL r
                    carry = if view (flag Carry) rs then 1 else 0
                    carry' = Bits.testBit orig 0
                    res = Bits.shiftR orig 1 + Bits.shiftL carry 7
                in
                    rs
                        & set (flag Carry) carry'
                            . set (flag Zero) (res == 0)
                            . clearFlag HalfCarry
                            . clearFlag Negative
                            . set (targetL r) res
        RR_derefHL -> exec $ do
            s <- get
            let
                addr = s ^. registers % hl
                carry = if hasFlag Carry s then 1 else 0
                orig = readByte (view memoryBus s) addr
                carry' = Bits.testBit orig 0
                res = Bits.shiftR orig 1 + Bits.shiftL carry 7
            writeMemory addr res
            modifying' registers $
                set (flag Carry) carry'
                    . set (flag Zero) (res == 0)
                    . clearFlag HalfCarry
                    . clearFlag Negative
        RRC r -> exec $ do
            modifying' registers $ \rs ->
                let
                    orig = rs ^. targetL r
                    carry = Bits.testBit orig 0
                    res = Bits.rotateR orig 1
                in
                    rs
                        & set (flag Carry) carry
                            . set (flag Zero) (res == 0)
                            . clearFlag HalfCarry
                            . clearFlag Negative
                            . set (targetL r) res
        RRC_derefHL -> exec $ do
            s <- get
            let
                addr = s ^. registers % hl
                orig = readByte (view memoryBus s) addr
                carry = Bits.testBit orig 0
                res = Bits.rotateR orig 1
            writeMemory addr res
            modifying' registers $
                set (flag Carry) carry
                    . set (flag Zero) (res == 0)
                    . clearFlag HalfCarry
                    . clearFlag Negative
        RL r -> exec $ do
            modifying' registers $ \rs ->
                let
                    orig = rs ^. targetL r
                    carry = if view (flag Carry) rs then 1 else 0
                    carry' = Bits.testBit orig 7
                    res = Bits.shiftL orig 1 + carry
                in
                    rs
                        & set (flag Carry) carry'
                            . set (flag Zero) (res == 0)
                            . clearFlag HalfCarry
                            . clearFlag Negative
                            . set (targetL r) res
        RL_derefHL -> exec $ do
            s <- get
            let
                addr = s ^. registers % hl
                orig = readByte (view memoryBus s) addr
                carry = if hasFlag Carry s then 1 else 0
                carry' = Bits.testBit orig 7
                res = Bits.shiftL orig 1 + carry
            writeMemory addr res
            modifying' registers $
                set (flag Carry) carry'
                    . set (flag Zero) (res == 0)
                    . clearFlag HalfCarry
                    . clearFlag Negative
        RLC r -> exec $ do
            modifying' registers $ \rs ->
                let
                    orig = rs ^. targetL r
                    carry = Bits.testBit orig 7
                    res = Bits.rotateL orig 1
                in
                    rs
                        & set (flag Carry) carry
                            . set (flag Zero) (res == 0)
                            . clearFlag HalfCarry
                            . clearFlag Negative
                            . set (targetL r) res
        RLC_derefHL -> exec $ do
            s <- get
            let
                addr = s ^. registers % hl
                orig = readByte (view memoryBus s) addr
                carry = Bits.testBit orig 7
                res = Bits.rotateL orig 1
            writeMemory addr res
            modifying' registers $
                set (flag Carry) carry
                    . set (flag Zero) (res == 0)
                    . clearFlag HalfCarry
                    . clearFlag Negative
        SLA r -> exec $ do
            modifying' registers $ \rs ->
                let
                    orig = rs ^. targetL r
                    carry = Bits.testBit orig 7
                    res = Bits.shiftL orig 1
                in
                    rs
                        & set (flag Carry) carry
                            . set (flag Zero) (res == 0)
                            . clearFlag HalfCarry
                            . clearFlag Negative
                            . set (targetL r) res
        SLA_derefHL -> exec $ do
            s <- get
            let
                addr = s ^. registers % hl
                orig = readByte (view memoryBus s) addr
                carry = Bits.testBit orig 7
                res = Bits.shiftL orig 1
            writeMemory addr res
            modifying' registers $
                set (flag Carry) carry
                    . set (flag Zero) (res == 0)
                    . clearFlag HalfCarry
                    . clearFlag Negative
        SRA r -> exec $ do
            modifying' registers $ \rs ->
                let
                    orig = rs ^. targetL r
                    carry = Bits.testBit orig 0
                    msb = orig .&. 0x80
                    res = Bits.shiftR orig 1 + msb
                in
                    rs
                        & set (flag Carry) carry
                            . set (flag Zero) (res == 0)
                            . clearFlag HalfCarry
                            . clearFlag Negative
                            . set (targetL r) res
        SRA_derefHL -> exec $ do
            s <- get
            let
                addr = s ^. registers % hl
                orig = readByte (view memoryBus s) addr
                carry = Bits.testBit orig 0
                msb = orig .&. 0x80
                res = Bits.shiftR orig 1 + msb
            writeMemory addr res
            modifying' registers $
                set (flag Carry) carry
                    . set (flag Zero) (res == 0)
                    . clearFlag HalfCarry
                    . clearFlag Negative
        DAA ->
            exec $ daa
        SCF -> exec $ do
            modifying' registers $ \rs ->
                rs
                    & clearFlag Negative
                        . clearFlag HalfCarry
                        . setFlag Carry
        CCF -> exec $ do
            modifying' registers $ \rs ->
                rs
                    & clearFlag Negative
                        . clearFlag HalfCarry
                        . set (flag Carry) (not $ view (flag Carry) rs)
        RLCA -> exec $ do
            modifying' registers $ \rs ->
                let
                    orig = rs ^. a
                    res = Bits.rotateL orig 1
                    needsCarry = Bits.testBit orig 7
                in
                    rs
                        & clearFlag Zero
                            . clearFlag Negative
                            . clearFlag HalfCarry
                            . set (flag Carry) needsCarry
                            . set a res
        RRCA -> exec $ do
            modifying' registers $ \rs ->
                let
                    orig = rs ^. a
                    res = Bits.rotateR orig 1
                    needsCarry = Bits.testBit orig 0
                in
                    rs
                        & clearFlag Zero
                            . clearFlag Negative
                            . clearFlag HalfCarry
                            . set (flag Carry) needsCarry
                            . set a res
        HALT -> do
            assign' halted True
            pure 0
        STOP -> do
            assign' halted True
            pure 0
  where
    exec action = action >> pure _baseCycles

add_sp :: I8 -> GameBoy ()
add_sp n = do
    modifying' registers $ \rs ->
        let (res, needsHalfCarry, needsCarry) = add_spPure rs._sp n
        in rs
            & clearFlag Negative
                . clearFlag Zero
                . set (flag HalfCarry) needsHalfCarry
                . set (flag Carry) needsCarry
                . set sp res

add_spPure :: U16 -> I8 -> (U16, Bool, Bool)
add_spPure orig n =
    let
        n' = toU8 n
        res' = fromIntegral @_ @I32 orig + fromIntegral n
        needsHalfCarry = toU8 (orig .&. 0xf) + n' .&. 0xf > 0xf
        needsCarry = orig .&. 0xff + toU16 n' > 0xff
    in
        (fromIntegral res', needsHalfCarry, needsCarry)

daa :: GameBoy ()
daa = do
    modify' $ \s ->
        let
            orig = s ^. registers % a
            halfCarry = hasFlag HalfCarry s
            carry = hasFlag Carry s
            negative = hasFlag Negative s
            u =
                if halfCarry || (not negative && (orig .&. 0xf) > 9)
                    then 6
                    else 0
            (u', setCarry) =
                if carry || not negative && orig > 0x99
                    then (u .|. 0x60, True)
                    else (u, False)
            res = orig + if negative then (-u') else u'
        in
            s
                & registers
                    %!~ set a res
                    . set (flag Zero) (res == 0)
                    . set (flag Carry) setCarry
                    . clearFlag HalfCarry

add_hl :: Lens' Registers U16 -> GameBoy ()
add_hl rr = do
    modifying' registers $ \rs ->
        let
            orig = view hl rs
            val = view rr rs
            res' = fromIntegral @_ @U32 orig + fromIntegral val
            res = fromIntegral res'
            needsCarry = res' > 0xffff
            needsHalfCarry = fromIntegral val .&. 0xfff > res' .&. 0xfff
        in
            rs
                & hl !~ res
                & clearFlag Negative
                & set (flag HalfCarry) needsHalfCarry
                & set (flag Carry) needsCarry

or_a :: TargetRegister -> GameBoy ()
or_a r = do
    modifying' registers $ \rs ->
        let
            val = rs ^. targetL r
            res = rs ^. a .|. val
        in
            rs
                & set a res
                    . clearFlag Negative
                    . clearFlag Carry
                    . clearFlag HalfCarry
                    . set (flag Zero) (res == 0)

or_a_u8 :: U8 -> GameBoy ()
or_a_u8 n = do
    modifying' registers $ \rs ->
        let res = rs ^. a .|. n
        in rs
            & set a res
                . clearFlag Negative
                . clearFlag Carry
                . clearFlag HalfCarry
                . set (flag Zero) (res == 0)

and_a :: TargetRegister -> GameBoy ()
and_a r = do
    modifying' registers $ \rs ->
        let
            val = rs ^. targetL r
            res = rs ^. a .&. val
        in
            rs
                & set a res
                    . clearFlag Negative
                    . clearFlag Carry
                    . setFlag HalfCarry
                    . set (flag Zero) (res == 0)

and_a_u8 :: U8 -> GameBoy ()
and_a_u8 n = do
    modifying' registers $ \rs ->
        let res = rs ^. a .&. n
        in rs
            & set a res
                . clearFlag Negative
                . clearFlag Carry
                . setFlag HalfCarry
                . set (flag Zero) (res == 0)

add_a :: TargetRegister -> GameBoy ()
add_a r = do
    modifying' registers $ \rs ->
        let
            orig = rs ^. a
            val = rs ^. targetL r
            res' = toU16 orig + toU16 val
            res = fromIntegral res'
            needsHalfCarry = (orig .&. 0x0f) + (val .&. 0x0f) > 0x0f
            needsCarry = res' > 0xff
        in
            rs
                & set a res
                    . clearFlag Negative
                    . set (flag Carry) needsCarry
                    . set (flag HalfCarry) needsHalfCarry
                    . set (flag Zero) (res == 0)

add_a_u8 :: U8 -> GameBoy ()
add_a_u8 n = do
    modifying' registers $ \rs ->
        let
            orig = rs ^. a
            res' = toU16 orig + toU16 n
            res = fromIntegral res'
            needsHalfCarry = (orig .&. 0x0f) + (n .&. 0x0f) > 0x0f
            needsCarry = res' > 0xff
        in
            rs
                & set a res
                    . clearFlag Negative
                    . set (flag Carry) needsCarry
                    . set (flag HalfCarry) needsHalfCarry
                    . set (flag Zero) (res == 0)

xor_a :: TargetRegister -> GameBoy ()
xor_a r = do
    modifying' registers $ \rs ->
        let
            val = rs ^. targetL r
            res = rs ^. a `Bits.xor` val
        in
            rs
                & set a res
                    . clearFlag Negative
                    . clearFlag Carry
                    . clearFlag HalfCarry
                    . set (flag Zero) (res == 0)

xor_a_u8 :: U8 -> GameBoy ()
xor_a_u8 n = do
    modifying'
        registers
        ( \rs ->
            let res = view a rs `Bits.xor` n
            in rs
                & flag Zero !~ (res == 0)
                & clearFlag Negative
                & clearFlag HalfCarry
                & clearFlag Carry
                & a !~ res
        )

cp_a :: TargetRegister -> GameBoy ()
cp_a r = do
    modifying' registers $ \rs ->
        let
            orig = rs ^. a
            val = rs ^. targetL r
            needsHalfCarry = orig .&. 0x0f < val .&. 0x0f
            res = orig - val
        in
            rs
                & setFlag Negative
                    . set (flag Carry) (orig < val)
                    . set (flag HalfCarry) needsHalfCarry
                    . set (flag Zero) (res == 0)

cp_a_u8 :: U8 -> GameBoy ()
cp_a_u8 n = do
    modify' $ \s ->
        let
            orig = s ^. registers % a
            res = orig - n
            needsHalfCarry = orig .&. 0x0f < n .&. 0x0f
        in
            s
                & registers
                    %!~ set (flag Zero) (res == 0)
                    . setFlag Negative
                    . set (flag HalfCarry) needsHalfCarry
                    . set (flag Carry) (orig < n)

sbc_a :: TargetRegister -> GameBoy ()
sbc_a r = do
    modifying' registers $ \rs ->
        let
            orig = rs ^. a
            carry = if rs ^. flag Carry then 1 else 0
            n = rs ^. targetL r
            val = n + carry
            res = orig - val
            needsHalfCarry = toI16 orig .&. 0xf - toI16 n .&. 0xf - toI16 carry < 0
            needsCarry = toI16 orig - toI16 n - toI16 carry < 0
        in
            rs
                & set a res
                    . setFlag Negative
                    . set (flag Carry) needsCarry
                    . set (flag HalfCarry) needsHalfCarry
                    . set (flag Zero) (res == 0)

sbc_a_u8 :: U8 -> GameBoy ()
sbc_a_u8 n = do
    modifying' registers $ \rs ->
        let
            orig = rs ^. a
            carry = if rs ^. flag Carry then 1 else 0
            val = n + carry
            res = orig - val
            needsHalfCarry = toI16 orig .&. 0xf - toI16 n .&. 0xf - toI16 carry < 0
            needsCarry = toI16 orig - toI16 n - toI16 carry < 0
        in
            rs
                & set a res
                    . setFlag Negative
                    . set (flag Carry) needsCarry
                    . set (flag HalfCarry) needsHalfCarry
                    . set (flag Zero) (res == 0)

sub_a :: TargetRegister -> GameBoy ()
sub_a r = do
    modifying' registers $ \rs ->
        let
            orig = rs ^. a
            val = rs ^. targetL r
            res = rs ^. a - val
            needsHalfCarry = orig .&. 0x0f < val .&. 0x0f
        in
            rs
                & set a res
                    . setFlag Negative
                    . set (flag Carry) (orig < val)
                    . set (flag HalfCarry) needsHalfCarry
                    . set (flag Zero) (res == 0)

sub_a_u8 :: U8 -> GameBoy ()
sub_a_u8 n = do
    modifying' registers $ \rs ->
        let
            orig = rs ^. a
            res = orig - n
            needsHalfCarry = orig .&. 0x0f < n .&. 0x0f
        in
            rs
                & set a res
                    . setFlag Negative
                    . set (flag Carry) (orig < n)
                    . set (flag HalfCarry) needsHalfCarry
                    . set (flag Zero) (res == 0)

adc_a :: TargetRegister -> GameBoy ()
adc_a r = do
    modifying' registers $ \rs ->
        let
            orig = rs ^. a
            val = rs ^. targetL r
            carry = if rs ^. flag Carry then 1 else 0
            res' = toU16 orig + toU16 val + toU16 carry
            needsCarry = res' > 0xff
            res = fromIntegral res'
            needsHalfCarry = (orig .&. 0x0f) + (val .&. 0x0f) + carry > 0x0f
        in
            rs
                & set a res
                    . clearFlag Negative
                    . set (flag Carry) needsCarry
                    . set (flag HalfCarry) needsHalfCarry
                    . set (flag Zero) (res == 0)

adc_a_u8 :: U8 -> GameBoy ()
adc_a_u8 n = do
    modifying' registers $ \rs ->
        let
            orig = rs ^. a
            carry = if rs ^. flag Carry then 1 else 0
            res' = toU16 orig + toU16 n + toU16 carry
            needsCarry = res' > 0xff
            res = fromIntegral res'
            needsHalfCarry = (orig .&. 0x0f) + (n .&. 0x0f) + carry > 0x0f
        in
            rs
                & set a res
                    . clearFlag Negative
                    . set (flag Carry) needsCarry
                    . set (flag HalfCarry) needsHalfCarry
                    . set (flag Zero) (res == 0)

updateTimers :: Int -> GameBoy ()
updateTimers cycles = do
    updateDivider cycles
    s <- get
    when (s ^. memoryBus % timerEnable) $ do
        let counter' = view timerCounter s - cycles
        assign' timerCounter counter'
        when (counter' <= 0) $ do
            freq <- use (memoryBus % timerFrequency)
            assign' timerCounter (counterFromFrequency freq)
            if s ^. memoryBus % tima == maxBound
                then do
                    assign' (memoryBus % tima) (s ^. memoryBus % tma)
                    assign' (memoryBus % timerIntRequested) True
                else modifying' (memoryBus % tima) (+ 1)

updateDivider :: Int -> GameBoy ()
updateDivider cycles = do
    counter <- use dividerCounter
    let counter' = counter + cycles
    assign' dividerCounter counter'
    when (counter' >= 255) $ do
        assign' dividerCounter 0
        modifying' (memoryBus % divider) (+ 1)

data TimerFrequency
    = Freq4K
    | Freq16K
    | Freq64K
    | Freq256K
    deriving (Eq, Show)

readTimerFrequency :: U8 -> TimerFrequency
readTimerFrequency n =
    case (n ^. bit 1, n ^. bit 0) of
        (False, False) -> Freq4K
        (False, True) -> Freq256K
        (True, False) -> Freq64K
        (True, True) -> Freq16K

counterFromFrequency :: TimerFrequency -> Int
counterFromFrequency = \case
    Freq4K -> 1024
    Freq16K -> 256
    Freq64K -> 64
    Freq256K -> 16

timerFrequency :: Getter MemoryBus TimerFrequency
timerFrequency = tac % to readTimerFrequency

handleInterrupts :: GameBoy Int
handleInterrupts = do
    s <- get
    if view masterInterruptEnable s
        then do
            let
                enabledInterrupts = s ^. memoryBus % ie
                requestedInterrupts = s ^. memoryBus % interruptFlags
            case findInterrupt (filter (Bits.testBit requestedInterrupts) [0 .. 4]) enabledInterrupts of
                Nothing -> pure 0
                Just interrupt -> do
                    handleInterrupt interrupt
                    pure 20
        else -- TODO: check whether 0 is correct; shouldn't it take cycles to
        -- check memory?
            pure 0
  where
    findInterrupt requestedInterrupts enabledInterrupts =
        case requestedInterrupts of
            [] -> Nothing
            (x : xs) ->
                if Bits.testBit enabledInterrupts x
                    then Just x
                    else findInterrupt xs enabledInterrupts
    handleInterrupt interrupt = do
        -- traceM $ "      INTERRUPT " <> show interrupt
        assign' masterInterruptEnable False
        assign' (memoryBus % interruptFlags % bit interrupt) False
        counter <- use programCounter
        push counter
        case interrupt of
            0 -> assign' programCounter 0x40 -- VBlank
            1 -> assign' programCounter 0x48 -- LCD stat
            2 -> assign' programCounter 0x50 -- Timer
            3 -> assign' programCounter 0x58 -- Serial
            4 -> assign' programCounter 0x60 -- Joypad
            s -> error $ "unhandled interrupt: " <> show s

dmaTransfer :: U8 -> GameBoy ()
dmaTransfer n = do
    let startAddr :: U16 = Bits.shiftL (fromIntegral n) 8 -- times 0x100
    bus <- use memoryBus
    -- TODO: use a slice pointing to cartridge memory instead?
    forM_ ([0 .. 0xa0 - 1] :: [U16]) $ \i ->
        assign' (memoryBus % oam % byte i) (readByte bus (startAddr + i))
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

readTileRow :: MemoryBus -> FlipMode -> U16 -> Int -> Vector Color
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
        ypos = if useWindow then currentLine - wy else currentLine + y
        vertTileIndexOffset = (toU16 ypos .>>. 3) .<<. 5
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
                        tileColors = readTileRow bus NoFlip tileAddr (fromIntegral rowIndex)
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

drawScanline :: GameBoy ()
drawScanline = do
    bus <- use memoryBus
    let scanlineColors = readScanlineColors bus
    modifying'
        screen
        ( Vector.//
            [(scanlineColors._index, scanlineColors._colors)]
        )

readFlipMode :: U8 -> FlipMode
readFlipMode spriteAttrs =
    case (Bits.testBit spriteAttrs 5, Bits.testBit spriteAttrs 6) of
        (False, False) -> NoFlip
        (True, False) -> FlipX
        (False, True) -> FlipY
        _ -> FlipBoth

drawSprites :: GameBoy ()
drawSprites = do
    bus <- use memoryBus
    forM_ ([0 .. 39] :: [U16]) $ \sprite -> do
        let
            spriteIndex = sprite * 4 -- 4 bytes per sprite
            y = bus ^. oam % byte spriteIndex - 16
            x = bus ^. oam % byte (spriteIndex + 1) - 8
            tileOffset = bus ^. oam % byte (spriteIndex + 2)
            attrs = bus ^. oam % byte (spriteIndex + 3)
            flipMode = readFlipMode attrs
            currentLine = fromIntegral $ bus ^. scanline
            height = if (bus ^. spriteUsesTwoTiles) then 16 else 8
            line = currentLine - y
        when (line >= 0 && line < height) $ do
            -- FIXME: transparency!
            let
                tileMemStart = 0x8000 + 16 * fromIntegral tileOffset
                tileRow = readTileRow bus flipMode tileMemStart (fromIntegral line)
                dummyPalette = readByte bus $ if Bits.testBit attrs 4 then 0xff49 else 0xff48
            modifying'
                screen
                ( \scr ->
                    let
                        v = scr Vector.! fromIntegral currentLine
                        v' =
                            v
                                Vector.// ( fmap
                                                ( \i ->
                                                    ( fromIntegral x + i
                                                    , fmap (translateTileColors dummyPalette) tileRow Vector.! i
                                                    )
                                                )
                                                [0 .. 7]
                                          )
                    in
                        scr Vector.// [(fromIntegral currentLine, v')]
                )

setLcdStatus :: GameBoy ()
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
                -- FIXME/TODO: check BG priority here!
                drawScanline
                when (s ^. memoryBus % objEnabled) drawSprites
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

updateGraphics :: Int -> GameBoy ()
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
                if
                    | line == 144 -> do
                        prepared <- use screen
                        assign' preparedScreen prepared
                        assign' screen emptyScreen
                        assign' (memoryBus % interruptFlags % bit 0) True
                    | line > 153 -> do
                        assign' (memoryBus % scanline) 0
                    | otherwise -> pure ()

tileSize = 6

renderScreen :: MonadIO m => SDL.Renderer -> InMemoryScreen -> m ()
renderScreen renderer scr = do
    SDL.clear renderer
    traverse_
        ( \(y, line) ->
            traverse_
                ( \(x, color) -> do
                    -- NOTE: this is VERY slow
                    let rect =
                            SDL.Rectangle
                                (SDL.P $ SDL.V2 (tileSize * fromIntegral x) (tileSize * fromIntegral y))
                                (SDL.V2 tileSize tileSize)
                    SDL.rendererDrawColor renderer SDL.$= getColor color
                    SDL.fillRect renderer (Just rect)
                )
                (Vector.indexed line)
        )
        (Vector.indexed scr)
    SDL.present renderer
  where
    getColor = \case
        0 -> SDL.V4 155 188 15 0xff
        1 -> SDL.V4 139 172 15 0xff
        2 -> SDL.V4 48 98 48 0xff
        3 -> SDL.V4 15 56 15 0xff
        _ -> error "impossible color"

runGraphics :: (GamepadState -> IO ()) -> IORef InMemoryScreen -> IO ()
runGraphics inputCallback scrRef = do
    withSdl $ withSdlWindow $ \w ->
        withSdlRenderer w $ \renderer -> do
            now <- Time.getCurrentTime
            void $ appLoop renderer now 0
  where
    escPressed evt =
        case SDL.eventPayload evt of
            SDL.KeyboardEvent keyboardEvent ->
                SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
                    && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeEscape
            _ -> False
    collectButtonPresses =
        foldl'
            ( \buttonActions evt ->
                case SDL.eventPayload evt of
                    SDL.KeyboardEvent keyboardEvent ->
                        let
                            keycode = SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent)
                            change =
                                if SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
                                    then Set.insert
                                    else Set.delete
                        in
                            -- FIXME: looks like it "forgets" buttons when
                            -- multiple are being pressed
                            case keycode of
                                SDL.KeycodeDown -> change BtnDown buttonActions
                                SDL.KeycodeUp -> change BtnUp buttonActions
                                SDL.KeycodeLeft -> change BtnLeft buttonActions
                                SDL.KeycodeRight -> change BtnRight buttonActions
                                SDL.KeycodeZ -> change BtnB buttonActions
                                SDL.KeycodeX -> change BtnA buttonActions
                                SDL.KeycodeI -> change BtnStart buttonActions
                                SDL.KeycodeN -> change BtnSelect buttonActions
                                _ -> buttonActions
                    _ -> buttonActions
            )
            Set.empty
    appLoop renderer t (frames :: Int) = do
        evts <- SDL.pollEvents
        let buttons = collectButtonPresses evts
        inputCallback buttons
        SDL.clear renderer
        scr <- readIORef scrRef
        renderScreen renderer scr
        SDL.present renderer
        unless (any escPressed evts) $ do
            if frames == 59
                then do
                    now <- Time.getCurrentTime
                    let
                        dt = Time.diffUTCTime now t
                        fps = 60 / dt
                    putStrLn $ "        RENDER FPS: " <> show fps
                    appLoop renderer now 0
                else appLoop renderer t (frames + 1)

withSdl :: IO a -> IO a
withSdl =
    Exception.bracket_
        ( do
            SDL.initialize @[] [SDL.InitVideo]
            putStrLn "SDL initialized"
        )
        ( do
            SDL.quit
            putStrLn "Shut down SDL"
        )

withSdlWindow :: (SDL.Window -> IO a) -> IO a
withSdlWindow action = do
    let windowConfig = SDL.defaultWindow{SDL.windowInitialSize = SDL.V2 (160 * tileSize) (144 * tileSize)}
    Exception.bracket
        (SDL.createWindow "GameBoy emulator" windowConfig)
        ( \w -> do
            SDL.destroyWindow w
            putStrLn "Window destroyed"
        )
        action

withSdlRenderer :: SDL.Window -> (SDL.Renderer -> IO a) -> IO a
withSdlRenderer window = do
    -- NOTE: without VSYNC we are dropping keypresses for some reason
    let rendererConfig = SDL.defaultRenderer{SDL.rendererType = SDL.AcceleratedVSyncRenderer}
    Exception.bracket
        (SDL.createRenderer window (-1) rendererConfig)
        ( \r -> do
            SDL.destroyRenderer r
            putStrLn "Renderer destroyed"
        )

mainLoop ::
    IORef InMemoryScreen ->
    STM.TVar GamepadState ->
    GameBoy ()
mainLoop scrRef buttonsRef = do
    now <- liftIO Time.getCurrentTime
    loop 0 now
  where
    loop (frames :: Int) t = do
        -- TODO: it feels wrong to start at 0 here
        -- shouldn't we use the superfluous cycles from the frame before?
        oneFrame 0
        scr <- use preparedScreen
        liftIO $ writeIORef scrRef scr
        if (frames == 59)
            then do
                now <- liftIO Time.getCurrentTime
                let
                    dt = Time.diffUTCTime now t
                    fps = realToFrac (frames + 1) / dt
                liftIO $ putStrLn $ "  FPS = " <> show fps
                loop 0 now
            else loop (frames + 1) t
    oneFrame n = when (n < maxCyclesPerFrame) $ do
        syncInput
        s <- get
        cycles <-
            if not $ s ^. halted
                then do
                    instr <- fetch
                    cycles <- execute instr
                    -- liftIO $ putStrLn $ toHex (view programCounter s) <> " :  " <> show instr
                    -- liftIO $ putStrLn $ " DIV == " <> toHex (view (memoryBus % timers % divider) s `Bits.shiftR` 8)
                    pure cycles
                else do
                    -- liftIO $ putStrLn "  [HALT]"
                    when (s ^. memoryBus % interruptFlags > 0) $
                        assign' halted False
                    pure 4
        updateTimers cycles
        updateGraphics cycles
        interruptCycles <- handleInterrupts
        oneFrame (n + cycles + interruptCycles)
    syncInput = do
        buttons <- liftIO $ STM.readTVarIO buttonsRef
        -- unless (Set.null buttons) (liftIO $ print buttons)
        assign' (memoryBus % gamepadState) buttons

-- snapshotBackgroundArea = do
--     bus <- use memoryBus
--     let
--         addr = case bus ^. bgTileMapArea of
--             Area9800 -> 0x9800
--             Area9C00 -> 0x9C00
--         tiles =
--             Vector.concatMap
--                 ( \y ->
--                     let rowTiles =
--                             fmap
--                                 ( \x ->
--                                     let
--                                         tileIdentifierAddr = addr + 32 * y + x
--                                         tileIdentifier = readByte bus tileIdentifierAddr
--                                         tileAddr = determineTileAddress tileIdentifier (bus ^. addressingMode)
--                                     in
--                                         readTile bus NoFlip tileAddr
--                                 )
--                                 (Vector.fromList [0 .. 31])
--                     in Vector.foldl1' (\v w -> Vector.zipWith (Vector.++) v w) rowTiles
--                 )
--                 (Vector.fromList [0 .. 31])
--         colorToU8 = \case
--             Color0 -> 0 :: U8
--             Color1 -> 1
--             Color2 -> 2
--             Color3 -> 3
--     pure $ fmap (fmap colorToU8) tiles

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    args <- Environment.getArgs
    case args of
        [] -> fail "need path to ROM as first argument"
        (cartridgePath : _) -> do
            scrRef <- newIORef emptyScreen
            buttonsRef <- STM.newTVarIO noButtonsPressed
            game <- Async.async $ do
                bus <- initializeMemoryBus cartridgePath
                void $ execStateT (mainLoop scrRef buttonsRef) (mkInitialState bus)
            graphics <-
                Async.asyncBound $
                    runGraphics (STM.atomically . STM.writeTVar buttonsRef) scrRef
            void $ Async.waitAnyCancel [graphics, game]
