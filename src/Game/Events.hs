------------------------------------------------------------------------------
-- | Utilities for dealing with SDL events.
module Game.Events
    (
      isQuit
    , isSpace
    , mouseMove
    , direction
    ) where

import Game.Prelude
------------------------------------------------------------------------------
import qualified Game.Direction as Direction
------------------------------------------------------------------------------
import Game.Direction (Direction)
import Graphics.UI.SDL.Events
import Graphics.UI.SDL.Keysym


------------------------------------------------------------------------------
isQuit :: Event -> Bool
isQuit event = case event of
    Quit      -> True
    KeyDown k -> symKey k == SDLK_q && KeyModLeftMeta `elem` symModifiers k
    _         -> False


------------------------------------------------------------------------------
isSpace :: Event -> Bool
isSpace event = case event of
    KeyDown k -> symKey k == SDLK_SPACE
    _         -> False


------------------------------------------------------------------------------
mouseMove :: Event -> Maybe (Word16, Word16, Int16, Int16)
mouseMove event = case event of
    MouseMotion a b c d -> Just (a, b, c, d)
    _                   -> Nothing


------------------------------------------------------------------------------
-- | Produces the change in direction the event indicates.
direction :: Event -> Direction
direction event = case event of
    KeyDown k -> keyToDirection (symKey k)
    KeyUp k   -> Direction.opposite $ keyToDirection (symKey k)
    _         -> mempty


------------------------------------------------------------------------------
keyToDirection :: SDLKey -> Direction
keyToDirection key = case key of
    SDLK_UP    -> Direction.up
    SDLK_DOWN  -> Direction.down
    SDLK_LEFT  -> Direction.left
    SDLK_RIGHT -> Direction.right
    _          -> mempty

