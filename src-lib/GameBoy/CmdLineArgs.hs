module GameBoy.CmdLineArgs where

import Options.Applicative

data Configuration = Configuration
    { withBios :: Bool
    , romPath :: FilePath
    , targetFps :: Int
    }
    deriving (Show)

configuration :: Parser Configuration
configuration =
    Configuration
        <$> switch
            ( short 'b'
                <> long "with-bios"
                <> help "Boot the DMG bios"
            )
        <*> strOption
            ( short 'r'
                <> long "rom-path"
                <> help "Path to ROM file"
                <> metavar "PATH"
            )
        <*> option
            auto
            ( long "fps"
                <> help "Target FPS"
                <> showDefault
                <> value 60
                <> metavar "INT"
            )

readConfiguration :: IO Configuration
readConfiguration =
    execParser opts
  where
    opts =
        info
            (configuration <**> helper)
            ( fullDesc
                <> progDesc "Start the emulator with a given ROM file"
                <> header "A GameBoy emulator"
            )
