{-# LANGUAGE ViewPatterns #-}
module Raster where

import Control.Concurrent.Async (mapConcurrently)
import Control.Lens
import Codec.Picture
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as L
import Data.Monoid (Any)
import Diagrams
import Diagrams.Backend.Rasterific

-- rewritten from Digrams.Backend.Rasterific to attempt getting
-- a performance gain from parallelization

rasterGif'
  :: TypeableFloat n
  => SizeSpec V2 n
  -> GifLooping
  -> PaletteOptions
  -> [(QDiagram Rasterific V2 n Any, Int)]
  -> IO (Either String ByteString)
rasterGif' sz gOpts pOpts ds = do
  rasters <- mapConcurrently (pure . over _1 (rasterRgb8 sz)) ds
  imgs <- mapConcurrently (pure . pal) rasters
  pure $ encodeGifImages gOpts imgs
  where
    pal (palettize pOpts -> (img, p), d) = (p, d, img)

animatedGif'
  :: TypeableFloat n
  => FilePath
  -> SizeSpec V2 n
  -> GifLooping
  -> GifDelay
  -> [QDiagram Rasterific V2 n Any]
  -> IO ()
animatedGif' outFile sz gOpts i ds = do
  result <- rasterGif' sz gOpts defaultPaletteOptions (map (,i) ds)
  case result of
    Right bs -> L.writeFile outFile bs
    Left e -> putStrLn e

