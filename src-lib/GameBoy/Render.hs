{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module GameBoy.Render where

import Control.Exception qualified as Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.Foldable
import Data.IORef
import Data.Set qualified as Set
import Data.Time qualified as Time
import Data.Vector qualified as Vector
import Foreign.Ptr (Ptr)
import SDL qualified
import SDL.Raw qualified

import GameBoy.Gamepad
import GameBoy.State

tileSize = 8

updateTexture :: MonadIO m => SDL.Texture -> Ptr SDL.Raw.PixelFormat -> InMemoryScreen -> m ()
updateTexture tex fmt scr = do
    oneDim <-
        Vector.mapM
            ( \line ->
                Vector.mapM
                    ( \c ->
                        let (red, green, blue, alpha) = getColor c
                        in SDL.Raw.mapRGBA fmt red green blue alpha
                    )
                    line
            )
            scr
    SDL.updateTexture
        tex
        Nothing
        ( BS.toStrict $
            BSB.toLazyByteString $
                Vector.foldl' (\builder next -> builder <> BSB.word32LE next) mempty $
                    Vector.concatMap id oneDim
        )
        (160 * 4)
    pure ()
  where
    getColor = \case
        0 -> (155, 188, 15, 0xff)
        1 -> (139, 172, 15, 0xff)
        2 -> (48, 98, 48, 0xff)
        3 -> (15, 56, 15, 0xff)
        _ -> error "impossible color"

runGraphics :: (GamepadState -> IO ()) -> IORef InMemoryScreen -> IO ()
runGraphics inputCallback scrRef = do
    withSdl $ withSdlWindow $ \w ->
        withSdlRenderer w $ \renderer -> do
            withScreenTexture renderer $ \tex -> do
                fmt <- SDL.Raw.allocFormat SDL.Raw.SDL_PIXELFORMAT_RGBA8888
                now <- Time.getCurrentTime
                void $ appLoop renderer tex fmt now 0
                SDL.Raw.freeFormat fmt
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
    appLoop renderer tex fmt t (frames :: Int) = do
        evts <- SDL.pollEvents
        let buttons = collectButtonPresses evts
        inputCallback buttons
        SDL.clear renderer
        scr <- readIORef scrRef
        updateTexture tex fmt scr
        SDL.copy renderer tex Nothing Nothing
        SDL.present renderer
        unless (any escPressed evts) $ do
            if frames == 59
                then do
                    now <- Time.getCurrentTime
                    let
                        dt = Time.diffUTCTime now t
                        fps = 60 / dt
                    putStrLn $ "        RENDER FPS: " <> show fps
                    appLoop renderer tex fmt now 0
                else appLoop renderer tex fmt t (frames + 1)

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

withScreenTexture :: SDL.Renderer -> (SDL.Texture -> IO a) -> IO a
withScreenTexture renderer = do
    Exception.bracket
        (SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStreaming (SDL.V2 160 144))
        ( \tex -> do
            SDL.destroyTexture tex
            putStrLn "Screen texture destroyed"
        )
