module Render ( drawMaze ) where

import Diagrams.Prelude
import Maze
import Graph (edges)

type C a = ( V a ~ V2
           , N a ~ Double
           , Renderable (Path V2 Double) a
           , Backend a V2 Double
           )

drawMaze :: C a => Maze -> Diagram a
drawMaze m = case m.cellShape of
  Square -> drawSquareMaze m
  Hex -> drawHexMaze m

styles :: C a => Diagram a -> Diagram a
styles d = lw 3.0 d
         & lc black
         & lineCap LineCapRound
         & bg white
         & center
         & pad 1.05

drawSquareMaze :: C a => Maze -> Diagram a
drawSquareMaze m = innerWalls <> border & styles
  where
    cellSize :: Double = 1.0 / fromIntegral (max m.width m.height)
    halfCell = cellSize / 2.0
    hseg = p2 (negate halfCell, 0.0) ~~ p2 (halfCell, 0.0)
    vseg = p2 (0.0, negate halfCell) ~~ p2 (0.0, halfCell)

    col i = i `mod` m.width
    row i = i `div` m.width

    mkSeg (i, j) = let (ri, ci, rj, cj) = (row i, col i, row j, col j)
                       x = p2 (fromIntegral ci * cellSize, fromIntegral ri * cellSize)
                       y = p2 (fromIntegral cj * cellSize, fromIntegral rj * cellSize)
                       p = (x + y) / 2.0 + p2 (halfCell, halfCell)
                   in position [(p, if ri == rj then vseg else hseg)]

    innerWalls = edges m.graph & fmap mkSeg & mconcat

    w = fromIntegral m.width * cellSize
    h = fromIntegral m.height * cellSize

    border = fromVertices [p2 (0.0, 0.0), p2 (w, 0.0), p2 (w, h), p2 (0.0, h), p2 (0.0, 0.0)]

drawHexMaze :: C a => Maze -> Diagram a
drawHexMaze m = innerWalls <> border & styles
  where
    -- from center of cell to a vertex (also the length of each hexagon side)
    outerRadius = 1.0 / (0.5 + 3.0 * fromIntegral m.width)
    -- from center of cell to middle of a side
    innerRadius = (sqrt 3.0 / 2.0) * outerRadius

    col i = i `mod` m.width
    row i = i `div` m.width
    coords_ i = (col i, row i)

    isEven n = n `mod` 2 == 0

    toX (c, _) = fromIntegral c * 1.5 * outerRadius
    toY (c, r) = 1.0 - (fromIntegral r * 2.0 * innerRadius + if isEven c then 0.0 else innerRadius)

    centerPoint_ z = V2 (toX z) (toY z)

    seg (i, j) = let a = centerPoint_ (coords_ i)
                     b = centerPoint_ (coords_ j)
                     mp = (a + b) / V2 2.0 2.0
                     p' = perp (a - b)
                     r' = p' ^. _r
                     r = outerRadius / (2.0 * r')
                     p = p' * V2 r r
                     c :: Point V2 Double = P $ mp + p
                     d :: Point V2 Double = P $ mp - p
                 in c ~~ d
    innerWalls = map seg (edges m.graph) & mconcat & lc black

    topSeg v = let a = v + V2 (outerRadius * (-0.5)) innerRadius
                   b = v + V2 (outerRadius * 0.5) innerRadius
               in P a ~~ P b
    bottomSeg v = let a = v + V2 (outerRadius * (-0.5)) (-innerRadius)
                      b = v + V2 (outerRadius * 0.5) (-innerRadius)
                  in P a ~~ P b
    topLeftSeg v = let a = v + V2 (-outerRadius) 0.0
                       b = v + V2 (outerRadius * (-0.5)) innerRadius
                   in P a ~~ P b
    bottomLeftSeg v = let a = v + V2 (-outerRadius) 0.0
                          b = v + V2 (outerRadius * (-0.5)) (-innerRadius)
                      in P a ~~ P b
    topRightSeg v = let a = v + V2 (outerRadius * 0.5) innerRadius
                        b = v + V2 outerRadius 0.0
                    in P a ~~ P b
    bottomRightSeg v = let a = v + V2 (outerRadius * 0.5) (-innerRadius)
                           b = v + V2 outerRadius 0.0
                       in P a ~~ P b

    topBorder = mconcat $ do
      i <- [0..m.width-1]
      let coord@(c,_) = coords_ i
          p = centerPoint_ coord
      if isEven c
      then [topLeftSeg p, topSeg p, topRightSeg p]
      else [topSeg p]

    bottomBorder = mconcat $ do
      i <- [(m.width*(m.height-1))..(m.width*m.height)-1]
      let coord@(c,_) = coords_ i
          p = centerPoint_ coord
      if isEven c
      then [bottomSeg p]
      else [bottomLeftSeg p, bottomSeg p, bottomRightSeg p]

    leftBorder = mconcat $ do
      i <- [r * m.width | r <- [0..m.height-1]]
      let p = centerPoint_ $ coords_ i
      [topLeftSeg p, bottomLeftSeg p]

    rightBorder = mconcat $ do
      i <- [r * m.width - 1 | r <- [1..m.height]]
      let p = centerPoint_ $ coords_ i
      [topRightSeg p, bottomRightSeg p]

    border = topBorder <> bottomBorder <> leftBorder <> rightBorder

