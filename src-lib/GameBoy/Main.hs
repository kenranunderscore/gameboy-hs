{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module GameBoy.Main (main) where

import Control.Concurrent.Async qualified as Async
import Control.Monad
import Control.Monad.State.Strict
import Data.IORef
import Data.Vector qualified as Vector
import Optics
import System.Environment qualified as Environment

import GameBoy.BitStuff
import GameBoy.CPU
import GameBoy.Memory
import GameBoy.PPU
import GameBoy.Render qualified as Render
import GameBoy.State

maxCyclesPerFrame :: Int
maxCyclesPerFrame = 69_905

mainLoop :: (MonadIO m, GameBoy m) => IORef InMemoryScreen -> m ()
mainLoop scrRef = forever $ do
    oneFrame 0
    liftIO $ putStrLn "    [FRAME FINISHED]"
    fullBG <- snapshotBackgroundArea
    -- scr <- use screen
    liftIO $ writeIORef scrRef fullBG
  where
    oneFrame n = when (n < maxCyclesPerFrame) $ do
        s <- get
        cycles <-
            if not $ s ^. halted
                then
                    ( do
                        instr <- fetch
                        cycles <- execute instr
                        liftIO $ putStrLn $ toHex (view programCounter s) <> " :  " <> show instr
                        pure cycles
                    )
                else do
                    liftIO $ putStrLn "  [HALT]"
                    when (s ^. memoryBus % interruptFlags > 0) $
                        assign' halted False
                    pure 4
        -- liftIO $ putStrLn $ " LCDC: " <> toHex (view (memoryBus % lcdc) s)
        -- liftIO $ putStrLn $ " MODE: " <> show (view (memoryBus % addressingMode) s)
        -- liftIO $ putStrLn $ " MAP: " <> show (view (memoryBus % bgTileMapArea) s)
        -- dumpRegisters
        void $ updateTimers cycles
        void $ updateGraphics cycles
        void $ handleInterrupts
        oneFrame (n + cycles)
    snapshotBackgroundArea = do
        bus <- use memoryBus
        let
            addr = case bus ^. bgTileMapArea of
                Area9800 -> 0x9800
                Area9C00 -> 0x9C00
            tiles =
                Vector.concatMap
                    ( \y ->
                        let rowTiles =
                                fmap
                                    ( \x ->
                                        let
                                            tileIdentifierAddr = addr + 32 * y + x
                                            tileIdentifier = readByte bus tileIdentifierAddr
                                            tileAddr = determineTileAddress tileIdentifier (bus ^. addressingMode)
                                        in
                                            readTile bus tileAddr
                                    )
                                    (Vector.fromList [0 .. 31])
                        in Vector.foldl1' (\v w -> Vector.zipWith (Vector.++) v w) rowTiles
                    )
                    (Vector.fromList [0 .. 31])
            colorToU8 = \case
                Color0 -> 0 :: U8
                Color1 -> 1
                Color2 -> 2
                Color3 -> 3
        pure $ fmap (fmap colorToU8) tiles

main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        [] -> fail "need path to ROM as first argument"
        (cartridgePath : _) -> do
            scrRef <- newIORef emptyScreen
            graphics <- Async.asyncBound $ Render.runGraphics scrRef
            game <- Async.async $ do
                bus <- initializeMemoryBus cartridgePath
                void $ execStateT (mainLoop scrRef) (mkInitialState bus)
            void $ Async.waitAnyCancel [graphics, game]

dumpRegisters :: (MonadIO m, GameBoy m) => m ()
dumpRegisters = use registers >>= liftIO . print
