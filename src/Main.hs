module Main where

import Control.Monad.Random.Strict
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Maze
import Graph

drawMaze :: Maze -> Diagram B
drawMaze m = (edges m.graph & fmap mkSeg & mconcat) <> border & lw 3.0
  where
    cellSize :: Double = 1.0 / fromIntegral m.width
    halfCell = cellSize / 2.0
    hseg = p2 (negate halfCell, 0.0) ~~ p2 (halfCell, 0.0)
    vseg = p2 (0.0, negate halfCell) ~~ p2 (0.0, halfCell)

    row i = i `div` m.width
    col i = i `mod` m.width

    offset_ = p2 (halfCell, halfCell) - p2 (0.5, 0.5)

    mkSeg (i, j) = let (ri, ci, rj, cj) = (row i, col i, row j, col j)
                       x = p2 (fromIntegral ci * cellSize, fromIntegral ri * cellSize)
                       y = p2 (fromIntegral cj * cellSize, fromIntegral rj * cellSize)
                       p = (x + y) / 2.0 + offset_
                   in position [(p, if ri == rj then vseg else hseg)]

    tl = p2 ((-0.5), 0.5)
    tr = p2 (0.5, 0.5)
    bl = p2 ((-0.5), (-0.5))
    br = p2 (0.5, (-0.5))

    border = (tl ~~ tr) <> (tr ~~ br) <> (br ~~ bl) <> (bl ~~ tl) & pad 1.05


main :: IO ()
main = do
  let gen = mkStdGen 9874374
      m = evalRand (generateMaze 40 40) gen

  let d = drawMaze m

  mainWith $ d

