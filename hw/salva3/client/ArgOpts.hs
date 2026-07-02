module ArgOpts where

{- cabal:
build-depends: base, optparse-applicative
-}
import Options.Applicative

data Config = Config
  { windowWidth :: Int
  , windowHeight :: Int
  , windowX :: Int
  , windowY :: Int
  , address :: String
  , port :: Int
  } deriving (Show)

getWindow opts =
  pure (windowWidth opts, windowHeight opts, windowX opts, windowY opts)

cfg :: Parser Config
cfg =
  Config
    <$> option
          auto
          (long "windowWidth"
             <> short 'w'
             <> metavar "Int"
             <> value 800
             <> help "Width of the canvas window.")
    <*> option
          auto
          (long "windowHeight"
             <> short 'h'
             <> metavar "Int"
             <> value 600
             <> help "Height of the canvas window.")
    <*> option
          auto
          (long "windowX"
             <> short 'x'
             <> metavar "Int"
             <> value 350
             <> help
                  "The x position of the tol-left corner of the canvas window.")
    <*> option
          auto
          (long "windowY"
             <> short 'y'
             <> metavar "Int"
             <> value 150
             <> help
                  "The y position of the tol-left corner of the canvas window.")
    <*> strOption
          (long "address"
             <> short 'a'
             <> metavar "String"
             <> value "127.0.0.1"
             <> help "Address of the server.")
    <*> option
          auto
          (long "port"
             <> short 'p'
             <> metavar "Int"
             <> value 10042
             <> help "The port used to connect to the server.")

opts :: ParserInfo Config
opts =
  info
    (cfg <**> helper)
    (fullDesc
       <> progDesc "A client part of the logoweb application."
       <> header "_____Logo webapp_____")

parseCmdArgs = execParser opts
