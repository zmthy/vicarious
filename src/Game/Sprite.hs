------------------------------------------------------------------------------
-- | Exposes the 'Sprite' type for placing images on screen.
module Game.Sprite
    (
    -- * Sprite type
      Sprite
    , image
    , position

    -- * Creating sprites
    , load

    -- * Rendering sprites
    , draw
    ) where

import Game.Prelude
------------------------------------------------------------------------------
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
------------------------------------------------------------------------------
import Graphics.UI.SDL (Surface, rectH, rectW)


------------------------------------------------------------------------------
data Sprite = Sprite
    { position :: (Int, Int)
    , size     :: (Int, Int)
    , image    :: Surface
    }


------------------------------------------------------------------------------
load :: String -> IO Sprite
load name = do
    img  <- Image.load ("media/" <> name <> ".png")
    rect <- SDL.getClipRect img
    return $ Sprite (0, 0) (rectW rect, rectH rect) img


------------------------------------------------------------------------------
-- | Draws the sprite to the surface.
draw :: Surface -> Sprite -> IO ()
draw screen sprite = void $ SDL.blitSurface (image sprite) Nothing screen $
    Just $ uncurry (uncurry SDL.Rect (position sprite)) (size sprite)

