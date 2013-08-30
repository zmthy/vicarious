------------------------------------------------------------------------------
-- | Manages the loading and grouping of the game's assets.
module Game.Assets where

import Game.Prelude
------------------------------------------------------------------------------
import Game.Sprite


------------------------------------------------------------------------------
data Assets = Assets
    { background :: Sprite
    }


------------------------------------------------------------------------------
loadAssets :: IO ()
loadAssets = return ()

