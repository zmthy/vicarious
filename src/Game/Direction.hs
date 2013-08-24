module Game.Direction
    (
    -- * Direction type
      Direction
    , horizontal
    , vertical

    -- * Directions
    , left
    , right
    , up
    , down

    -- * Transformations
    , opposite
    , movement
    ) where

import Game.Prelude hiding (left, right)
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | A direction with horizontal and vertical components.  The integer values
-- are either -1, 0, or 1.
data Direction = Direction
    { dhori :: Int
    , dvert :: Int
    } deriving (Eq)

instance Show Direction where
    show d =
        "Left: " <> show (d^.horizontal) <> " Right: " <> show (d^.vertical)

instance Monoid Direction where
    mempty = Direction 0 0
    mappend a b = vertical %~ signum . (b^.vertical +) $
        horizontal %~ signum . (b^.horizontal +) $ a

horizontal :: Lens' Direction Int
horizontal f dir = map (\h -> dir { dhori = signum h }) $ f (dhori dir)

vertical :: Lens' Direction Int
vertical f dir = map (\v -> dir { dvert = signum v }) $ f (dvert dir)


------------------------------------------------------------------------------
left :: Direction
left = Direction (-1) 0


------------------------------------------------------------------------------
right :: Direction
right = Direction 1 0


------------------------------------------------------------------------------
up :: Direction
up = Direction 0 (-1)


------------------------------------------------------------------------------
down :: Direction
down = Direction 0 1


------------------------------------------------------------------------------
opposite :: Direction -> Direction
opposite = (vertical %~ negate) . (horizontal %~ negate)


------------------------------------------------------------------------------
-- | Scales the direction to an appropriate movement size, and transforms the
-- direction into a pair.
movement :: Int -> Direction -> (Int, Int)
movement s dir = twimap (* s) (dir^.horizontal, dir^.vertical)

