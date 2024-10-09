module Main where

import Control.Monad.Random.Strict
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Maze
import Graph



drawMaze :: Maze -> Diagram B
drawMaze m = (edges m.graph & fmap mkSeg & mconcat) # lw 3.0
  where
    cellSize :: Double = 1.0 / fromIntegral m.width
    hseg = (P $ V2 (negate cellSize / 2.0) 0.0) ~~ (P $ V2 (cellSize / 2.0) 0.0)
    vseg = (P $ V2 0.0 (negate cellSize / 2.0)) ~~ (P $ V2 0.0 (cellSize / 2.0))

    row i = i `div` m.width
    col i = i `mod` m.width

    offset_ = (P $ V2 (cellSize / 2.0) (cellSize / 2.0)) - (P $ V2 0.5 0.5)

    mkSeg (i, j) = let (ri, ci, rj, cj) = (row i, col i, row j, col j)
                       x = P $ V2 (fromIntegral ci * cellSize) (fromIntegral ri * cellSize)
                       y = P $ V2 (fromIntegral cj * cellSize) (fromIntegral rj * cellSize)
                       p = (x + y) / 2.0 + offset_
                   in position [(p, if ri == rj then vseg else hseg)]


main :: IO ()
main = do
  let gen = mkStdGen 73
      m = evalRand (generateMaze 50 50) gen

  let d = drawMaze m

  mainWith $ d



