{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module GameBoy.Render where

import Control.Exception qualified as Exception
import Control.Monad
import Data.Foldable
import Data.IORef
import Data.Set qualified as Set
import Data.Time qualified as Time
import Data.Vector.Mutable qualified as MVector
import SDL (($=))
import SDL qualified

import GameBoy.Gamepad
import GameBoy.State

tileSize = 6

renderScreen :: SDL.Renderer -> InMemoryScreen -> IO ()
renderScreen renderer scr = do
    SDL.clear renderer
    forM_ [0 :: Int .. MVector.length scr - 1] $ \y -> do
        line <- MVector.read scr y
        forM_ [0 :: Int .. MVector.length line - 1] $ \x -> do
            color <- MVector.read line x
            -- NOTE: this is VERY slow
            let rect =
                    SDL.Rectangle
                        (SDL.P $ SDL.V2 (tileSize * fromIntegral x) (tileSize * fromIntegral y))
                        (SDL.V2 tileSize tileSize)
            SDL.rendererDrawColor renderer $= getColor color
            SDL.fillRect renderer (Just rect)
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
