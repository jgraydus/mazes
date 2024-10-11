module Main where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.Random.Strict (evalRand, mkStdGen)
import Control.Monad.Writer.Strict (runWriterT)
import Diagrams.Prelude
import Diagrams.Backend.Rasterific qualified as Rasterific
import Diagrams.Backend.SVG qualified as SVG
import Maze
import ProgramOptions
import Raster
import Render

run :: ProgramOpts -> IO ()
run opts = do

  let gen = mkStdGen 9874374
      c = opts.mazeOpts.columns
      r = opts.mazeOpts.rows

  --let (m, fs) = flip evalRand gen $ runWriterT (generateMaze Square c r)
  let (m, fs) = flip evalRand gen $ runWriterT (generateMaze Hex c r)

      w = opts.renderOpts.width
      h = opts.renderOpts.height
      sizeSpec = mkSizeSpec2D w h

  if opts.renderOpts.animate
  then do
    let filename = opts.renderOpts.name <> ".gif"
    animation <- mapConcurrently (\g -> pure $ drawMaze @Rasterific.B $ m { graph = g}) fs
    animatedGif' filename sizeSpec Rasterific.LoopingForever 10 animation
  else do
    let d :: Diagram SVG.B = drawMaze m
        filename = opts.renderOpts.name <> ".svg"
    SVG.renderSVG filename sizeSpec d

main :: IO ()
main = do
  opts <- parseProgramOpts
  run opts

