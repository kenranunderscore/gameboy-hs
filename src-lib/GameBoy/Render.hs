{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module GameBoy.Render where

import Control.Exception qualified as Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable
import Data.IORef
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Debug.Trace
import SDL (($=))
import SDL qualified

import GameBoy.CPU
import GameBoy.State (InMemoryScreen)

tileSize = 8

renderScreen :: MonadIO m => SDL.Renderer -> InMemoryScreen -> m ()
renderScreen renderer scr = do
    SDL.clear renderer
    traverse_
        ( \(y, line) ->
            traverse_
                ( \(x, color) -> do
                    let rect =
                            SDL.Rectangle
                                (SDL.P $ SDL.V2 (tileSize * fromIntegral x) (tileSize * fromIntegral y))
                                (SDL.V2 tileSize tileSize)
                    SDL.rendererDrawColor renderer $= getColor color
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

runGraphics :: (Set.Set Button -> IO ()) -> IORef InMemoryScreen -> IO ()
runGraphics btnCallback scrRef = do
    withSdl $ withSdlWindow $ \w ->
        withSdlRenderer w $ \renderer -> do
            SDL.rendererDrawColor renderer $= SDL.V4 0 0 255 255
            void $ appLoop renderer
  where
    escPressed evt =
        case SDL.eventPayload evt of
            SDL.KeyboardEvent keyboardEvent ->
                SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
                    && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeEscape
            _ -> False
    collectButtonPresses =
        foldl'
            ( \buttons evt ->
                case SDL.eventPayload evt of
                    SDL.KeyboardEvent keyboardEvent ->
                        if SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
                            then case SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) of
                                SDL.KeycodeDown -> Set.insert BtnDown buttons
                                SDL.KeycodeUp -> Set.insert BtnUp buttons
                                SDL.KeycodeLeft -> Set.insert BtnLeft buttons
                                SDL.KeycodeRight -> Set.insert BtnRight buttons
                                SDL.KeycodeZ -> Set.insert BtnB buttons
                                SDL.KeycodeX -> Set.insert BtnA buttons
                                SDL.KeycodeI -> Set.insert BtnStart buttons
                                SDL.KeycodeN -> Set.insert BtnSelect buttons
                                _ -> buttons
                            else buttons
                    _ -> buttons
            )
            Set.empty
    appLoop renderer = do
        evts <- SDL.pollEvents
        let buttons = collectButtonPresses evts
        -- btnCallback buttons -- FIXME
        SDL.clear renderer
        scr <- readIORef scrRef
        renderScreen renderer scr
        SDL.present renderer
        unless (any escPressed evts) (appLoop renderer)

withSdl :: IO a -> IO a
withSdl =
    Exception.bracket_
        ( do
            SDL.initialize [SDL.InitVideo]
            putStrLn "SDL initialized"
        )
        ( do
            SDL.quit
            putStrLn "Shut down SDL"
        )

withSdlWindow :: (SDL.Window -> IO a) -> IO a
withSdlWindow action = do
    let windowConfig = SDL.defaultWindow{SDL.windowInitialSize = SDL.V2 (160 * tileSize) (144 * tileSize)}
    -- let windowConfig = SDL.defaultWindow{SDL.windowInitialSize = SDL.V2 (255 * tileSize) (255 * tileSize)}
    Exception.bracket
        (SDL.createWindow "GameBoy emulator" windowConfig)
        ( \w -> do
            SDL.destroyWindow w
            putStrLn "Window destroyed"
        )
        action

withSdlRenderer :: SDL.Window -> (SDL.Renderer -> IO a) -> IO a
withSdlRenderer window = do
    let rendererConfig = SDL.defaultRenderer{SDL.rendererType = SDL.AcceleratedVSyncRenderer}
    Exception.bracket
        (SDL.createRenderer window (-1) rendererConfig)
        ( \r -> do
            SDL.destroyRenderer r
            putStrLn "Renderer destroyed"
        )
