module Image.CreateImage where

import qualified Graphics.Image as I
import qualified Graphics.Image.IO as I


test = do
  test1 <- I.readImageRGBA I.VU "Tiles/Tile1.png"
  test2 <- I.readImageRGBA I.VU "Tiles/Tile2.png" 
  I.writeImage "testImage.png" $ I.leftToRight test1 test2
