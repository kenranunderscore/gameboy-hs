{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module GameBoy.Render where

import Control.Exception qualified as Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable
import Data.IORef
import Data.Vector qualified as Vector
import SDL (($=))
import SDL qualified

import GameBoy.State (InMemoryScreen)

renderScreen :: MonadIO m => SDL.Renderer -> InMemoryScreen -> m ()
renderScreen renderer scr = do
    SDL.clear renderer
    traverse_
        ( \(y, line) ->
            traverse_
                ( \(x, color) -> do
                    let rect = SDL.Rectangle (SDL.P $ SDL.V2 (8 * fromIntegral x) (8 * fromIntegral y)) (SDL.V2 8 8)
                    SDL.rendererDrawColor renderer $= getColor color
                    SDL.fillRect renderer (Just rect)
                )
                (Vector.indexed line)
        )
        (Vector.indexed scr)
    SDL.present renderer
  where
    getColor = \case
        0 -> SDL.V4 230 230 230 0xff
        1 -> SDL.V4 120 0 0 0xff
        2 -> SDL.V4 0 120 0 0xff
        3 -> SDL.V4 0 0 120 0xff
        _ -> error "impossible color"

runGraphics :: IORef InMemoryScreen -> IO ()
runGraphics scrRef = do
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
    appLoop renderer = do
        evts <- SDL.pollEvents
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
withSdlWindow =
    Exception.bracket
        (SDL.createWindow "GameBoy emulator" SDL.defaultWindow)
        ( \w -> do
            SDL.destroyWindow w
            putStrLn "Window destroyed"
        )

withSdlRenderer :: SDL.Window -> (SDL.Renderer -> IO a) -> IO a
withSdlRenderer window =
    Exception.bracket
        (SDL.createRenderer window (-1) SDL.defaultRenderer)
        ( \r -> do
            SDL.destroyRenderer r
            putStrLn "Renderer destroyed"
        )
