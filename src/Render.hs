module Render ( drawMaze ) where

import Diagrams.Prelude
import Maze
import Graph (edges)

type C a = (V a ~ V2, N a ~ Double, Renderable (Path V2 Double) a, Backend a V2 Double)

border :: C a => Diagram a
border = square 1.0 & fc white

drawMaze :: C a => Maze -> Diagram a
drawMaze m = (edges m.graph & fmap mkSeg & mconcat) <> (border & pad 1.05)
             & lw 3.0
             & lineCap LineCapRound
             & lc black
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
