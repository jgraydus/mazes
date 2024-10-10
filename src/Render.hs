module Render ( drawMaze ) where

import Diagrams.Prelude
import Maze
import Graph (edges)

type C a = (V a ~ V2, N a ~ Double, Renderable (Path V2 Double) a, Backend a V2 Double)

drawMaze :: C a => Maze -> Diagram a
drawMaze m = innerWalls <> border
             & lw 3.0
             & lc black
             & lineCap LineCapRound
             & center
             & pad 1.05
  where
    cellSize :: Double = 1.0 / fromIntegral (max m.width m.height)
    halfCell = cellSize / 2.0
    hseg = p2 (negate halfCell, 0.0) ~~ p2 (halfCell, 0.0)
    vseg = p2 (0.0, negate halfCell) ~~ p2 (0.0, halfCell)

    row i = i `div` m.width
    col i = i `mod` m.width

    mkSeg (i, j) = let (ri, ci, rj, cj) = (row i, col i, row j, col j)
                       x = p2 (fromIntegral ci * cellSize, fromIntegral ri * cellSize)
                       y = p2 (fromIntegral cj * cellSize, fromIntegral rj * cellSize)
                       p = (x + y) / 2.0 + p2 (halfCell, halfCell)
                   in position [(p, if ri == rj then vseg else hseg)]

    innerWalls = edges m.graph & fmap mkSeg & mconcat

    w = fromIntegral m.width * cellSize
    h = fromIntegral m.height * cellSize


    border = fromVertices [p2 (0.0, 0.0), p2 (w, 0.0), p2 (w, h), p2 (0.0, h), p2 (0.0, 0.0)]

