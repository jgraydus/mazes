module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

c :: Diagram B
c = square 1 # lw thick

d :: Diagram B
d = (c # fc red)||| (c # fc blue)


example :: Diagram B
example = square 1
          # explodeTrail
          # zipWith lwN [0.0, 0.025, 0.025, 0.025]
          # mconcat

main :: IO ()
main = mainWith $ (example ||| example) === (example ||| example)

