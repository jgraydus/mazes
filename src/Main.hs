module Main where

import Control.Monad.Random.Strict (evalRand, mkStdGen)
import Control.Monad.Writer.Strict (runWriterT)
import Diagrams.Prelude
import Diagrams.Backend.Rasterific qualified as Rasterific
import Diagrams.Backend.SVG qualified as SVG
import Maze
import ProgramOptions
import Render

run :: ProgramOpts -> IO ()
run opts = do

  let gen = mkStdGen 9874374
      c = opts.mazeOpts.columns
      r = opts.mazeOpts.rows
      (m, fs) = flip evalRand gen $ runWriterT (generateMaze c r)

      w = opts.renderOpts.width
      h = opts.renderOpts.height
      sizeSpec = mkSizeSpec2D (Just w) (Just h)

  if opts.renderOpts.animate
  then do
    let animation = map (\g -> drawMaze @Rasterific.B $ m { graph = g}) fs
        filename = opts.renderOpts.name <> ".gif"
    Rasterific.animatedGif filename sizeSpec Rasterific.LoopingNever 10 animation
  else do
    let d :: Diagram SVG.B = drawMaze m
        filename = opts.renderOpts.name <> ".svg"
    SVG.renderSVG filename sizeSpec d

main :: IO ()
main = do
  opts <- parseProgramOpts
  run opts

