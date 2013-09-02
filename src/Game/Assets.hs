------------------------------------------------------------------------------
-- | Manages the loading and grouping of the game's assets.
module Game.Assets where

import Game.Prelude
------------------------------------------------------------------------------
import Game.Sprite


------------------------------------------------------------------------------
pixel :: Int
pixel = 4


------------------------------------------------------------------------------
fishStart :: Int
fishStart = 100


------------------------------------------------------------------------------
data Assets = Assets Background Paramedic Ghost


------------------------------------------------------------------------------
data Ghost = Ghost
    { fade  :: [Sprite]
    , rod   :: [Sprite]
    , cast  :: [Sprite]
    , pause :: Sprite
    , cast' :: Sprite
    , yank  :: Sprite
    , loss  :: [Sprite]
    , done  :: Sprite
    }


------------------------------------------------------------------------------
data Background = Background
    { static :: Sprite
    , light  :: Sprite
    , dark   :: Sprite
    , tape   :: Sprite
    }


------------------------------------------------------------------------------
data Paramedic = Paramedic
    { para1 :: Sprite
    , paraT :: Sprite
    , para2 :: Sprite
    }


------------------------------------------------------------------------------
loadAssets :: IO ()
loadAssets = return ()

