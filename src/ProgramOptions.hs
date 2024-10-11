module ProgramOptions (
    MazeOpts(..),
    parseProgramOpts,
    ProgramOpts(..),
    RenderOpts(..),
) where

import Options.Applicative
import Maze

data MazeOpts = MazeOpts
  { columns :: Int
  , rows :: Int
  , cellShape :: CellShape
  } deriving Show

parseCellShape :: ReadM CellShape
parseCellShape = eitherReader $ \case
  "square" -> Right Square
  "hex" -> Right Hex
  _ -> Left "not a valid shape argument"

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
  ) <*>
  option parseCellShape
  (  long "cellShape"
  <> short 's'
  <> help "The type of maze grid. Values are 'square' and 'hex'."
  <> value Square
  )

data RenderOpts = RenderOpts
  { width :: Maybe Double
  , height :: Maybe Double
  , animate :: Bool
  , name :: String
  } deriving Show

renderOptsParser :: Parser RenderOpts
renderOptsParser = RenderOpts <$>
  optional (option auto
  (  long "width"
  <> short 'w'
  <> help "The desired width of the output."
  )) <*>
  optional (option auto
  (  long "height"
  <> short 'h'
  <> help "The desired height of the output."
  )) <*>
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
    
