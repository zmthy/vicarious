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
    { spos   :: (Int, Int)
    , ssize  :: (Int, Int)
    , simage :: Surface
    }

position :: Lens' Sprite (Int, Int)
position f sprite = fmap (\p -> sprite { spos = p }) $ f (spos sprite)

size :: Lens' Sprite (Int, Int)
size f sprite = fmap (\s -> sprite { ssize = s }) $ f (ssize sprite)

image :: Lens' Sprite Surface
image f sprite = fmap (\i -> sprite { simage = i }) $ f (simage sprite)


------------------------------------------------------------------------------
load :: String -> IO Sprite
load name = do
    img  <- Image.load ("media/" <> name <> ".png")
    rect <- SDL.getClipRect img
    return $ Sprite (0, 0) (rectW rect, rectH rect) img


------------------------------------------------------------------------------
-- | Draws the sprite to the surface.
draw :: Surface -> Sprite -> IO ()
draw screen sprite = void $ SDL.blitSurface (sprite^.image) Nothing screen $
    Just $ uncurry (uncurry SDL.Rect (sprite^.position)) (sprite^.size)

