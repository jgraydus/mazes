module ProgramOptions (
    MazeOpts(..),
    parseProgramOpts,
    ProgramOpts(..),
    RenderOpts(..),
) where

import Options.Applicative

data MazeOpts = MazeOpts
  { columns :: Int
  , rows :: Int
  } deriving Show

mazeOptsParser :: Parser MazeOpts
mazeOptsParser = MazeOpts <$>
  option auto
  (  long "columns"
  <> short 'c'
  <> help "The number of columns of cells in the maze."
  <> value 10
  ) <*>
  option auto
  (  long "rows"
  <> short 'r'
  <> help "The number of rows of cells in the maze."
  <> value 10
  )

data RenderOpts = RenderOpts
  { width :: Double
  , height :: Double
  , animate :: Bool
  , name :: String
  } deriving Show

renderOptsParser :: Parser RenderOpts
renderOptsParser = RenderOpts <$>
  option auto
  (  long "width"
  <> short 'w'
  <> help "The desired width of the output."
  ) <*>
  option auto
  (  long "height"
  <> short 'h'
  <> help "The desired height of the output."
  ) <*>
  switch
  (  long "animate"
  <> short 'a'
  <> help "When set the output will be an animated gif demonstrating the construction of the maze."
  ) <*>
  strOption
  (  long "name"
  <> short 'n'
  <> help "A name for the image. Used to name the output file."
  <> value "out"
  )

data ProgramOpts = ProgramOpts
  { mazeOpts :: MazeOpts
  , renderOpts :: RenderOpts
  } deriving Show

programOptsParser :: Parser ProgramOpts
programOptsParser = ProgramOpts <$> mazeOptsParser <*> renderOptsParser

parseProgramOpts :: IO ProgramOpts
parseProgramOpts = execParser $
  info programOptsParser (fullDesc <> progDesc "Generate mazes")
    
