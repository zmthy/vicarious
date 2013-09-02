module Game.Data
    (
    -- * Types
      Game (Game)
    , Direction (..)

    -- * Lenses
    , player
    , direction
    , horizontal
    , vertical
    , fish
    ) where

import Game.Prelude
------------------------------------------------------------------------------
import Game.Sprite


------------------------------------------------------------------------------
data Game = Game
    { gplayer     :: Sprite
    , gdirection  :: Direction
    , ghorizontal :: Int16
    , gvertical   :: Int16
    , gfish       :: Int
    }

instance Show Game where
    show g = "Game dir " <> show (gdirection g) <> " hori " <>
        show (ghorizontal g) <> " vert " <> show (gvertical g)

player :: Lens' Game Sprite
player f game = map (\g -> game { gplayer = g }) $ f (gplayer game)

direction :: Lens' Game Direction
direction f game = map (\d -> game { gdirection = d }) $ f (gdirection game)

horizontal :: Lens' Game Int16
horizontal f game = map (\h -> game { ghorizontal = h }) $ f (ghorizontal game)

vertical :: Lens' Game Int16
vertical f game = map (\v -> game { gvertical = v }) $ f (gvertical game)

fish :: Lens' Game Int
fish f game = map (\x -> game { gfish = x }) $ f (gfish game)


------------------------------------------------------------------------------
-- | Current mouse direction required to advance.
data Direction = DUp | DRight | DDown | DLeft
    deriving (Eq, Show)

instance Enum Direction where
    fromEnum DUp    = 0
    fromEnum DRight = 1
    fromEnum DDown  = 2
    fromEnum DLeft  = 3

    toEnum i = case i `rem` 4 of
        0 -> DUp
        1 -> DRight
        2 -> DDown
        _ -> DLeft

