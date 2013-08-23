{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | Exposes the 'Sprite' type for placing images on screen.
module Game.Sprite
    (
      -- * Sprite type
      Sprite
    , image
    , position

      -- * Creating sprites
    , sprite
    ) where

import Game.Prelude
------------------------------------------------------------------------------
import qualified Graphics.UI.SDL as SDL
------------------------------------------------------------------------------
import Graphics.UI.SDL (Surface)
import Graphics.UI.SDL.Image (load)


------------------------------------------------------------------------------
data Sprite = Sprite
    { _position :: (Int, Int)
    , _size     :: (Int, Int)
    , _image    :: Surface
    }

makeLenses ''Sprite


------------------------------------------------------------------------------
sprite :: String -> IO ()
sprite name = do
    image <- load ("media/" <> name <> ".png")
    rect  <- SDL.getClipRect image
    return $ Sprite (0, 0) (rectW rect, rectH rect)


------------------------------------------------------------------------------
-- | Draws the sprite to the surface.
draw :: Surface -> Sprite -> IO ()
draw screen sprite = void $ SDL.blitSurface (sprite^.image) Nothing screen $
    Just $ uncurry (uncurry SDL.Rect (sprite^.position)) (sprite^.size)

