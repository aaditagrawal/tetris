-- | Compact static widget images before placing them in Brick's render cache.
module UI.Image (compact, compactImage, replaceRegion) where

import Brick (Widget(..), attrL, getContext, imageL)
import Control.Lens ((^.), (&), (.~))
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as Vector
import qualified Graphics.Vty as V
import Graphics.Vty.PictureToSpans (displayOpsForPic)
import Graphics.Vty.Span (SpanOp(..))

compact :: Widget n -> Widget n
compact widget = Widget (hSize widget) (vSize widget) $ do
  context <- getContext
  result <- render widget
  pure $ result & imageL .~ compactImage (context ^. attrL) (result ^. imageL)

-- | Resolve background padding, then merge adjacent text with equal attributes.
-- This is for opaque panels: their padding uses the supplied background attr.
compactImage :: V.Attr -> V.Image -> V.Image
compactImage background image = V.vertCat $ map rowImage $ Vector.toList ops
  where
    bounds = (V.imageWidth image, V.imageHeight image)
    picture = (V.picForImage image) { V.picBackground = V.Background ' ' background }
    ops = displayOpsForPic picture bounds
    rowImage = V.horizCat . map (uncurry V.text) . foldr merge [] . map textSpan . Vector.toList
    textSpan (TextSpan attr _ _ txt) = (attr, txt)
    textSpan (Skip n) = (background, TL.replicate (fromIntegral n) $ TL.singleton ' ')
    textSpan (RowEnd n) = (background, TL.replicate (fromIntegral n) $ TL.singleton ' ')
    merge (attr, txt) ((nextAttr, nextText):rest)
      | attr == nextAttr = (attr, TL.append txt nextText):rest
    merge spanText rest = spanText:rest

-- | Replace an opaque rectangular region without adding a picture layer.
replaceRegion :: (Int, Int) -> V.Image -> V.Image -> V.Image
replaceRegion (x, y) overlay background
  | width <= 0 || height <= 0 = background
  | otherwise = V.vertCat
      [ slice 0 0 totalWidth top background
      , V.horizCat
          [ slice 0 top left height background
          , slice (max 0 $ -x) (max 0 $ -y) width height overlay
          , slice (left + width) top (totalWidth - left - width) height background
          ]
      , slice 0 (top + height) totalWidth (totalHeight - top - height) background
      ]
  where
    totalWidth = V.imageWidth background
    totalHeight = V.imageHeight background
    left = max 0 x
    top = max 0 y
    width = min (totalWidth - left) (V.imageWidth overlay - max 0 (-x))
    height = min (totalHeight - top) (V.imageHeight overlay - max 0 (-y))
    slice column row w h = V.crop w h . V.translate (-column) (-row)
