------------------------------------------------------------------------------
-- | Main module.  Exposes the SDL main function, which sets up the game loop
-- and pushes events into the Netwire session.
module Main (main, sdlMain) where

import Game.Prelude
------------------------------------------------------------------------------
import qualified Control.Wire as Wire
import qualified Game.Direction as Direction
import qualified Game.Events as Events
import qualified Game.Sprite as Sprite
import qualified Graphics.UI.SDL as SDL
------------------------------------------------------------------------------
import Control.Wire (Session, Time, Wire)
import Game.Direction (Direction)
import Game.Sprite (Sprite)
import Graphics.UI.SDL (Event, Surface)


------------------------------------------------------------------------------
-- | The real main function.  Sets up SDL and starts the game.
sdlMain :: IO ()
sdlMain = SDL.withInit [SDL.InitVideo] $ do
    screen <- SDL.setVideoMode 800 600 0 [SDL.SWSurface]
    SDL.rawSetCaption (Just "Vicarious") Nothing
    ghost <- Sprite.load "ghost"
    tarmac <- Sprite.load "road/tarmac"
    let start = Game ghost tarmac mempty
    render screen [tarmac, ghost]
    eventLoop screen (Wire.mkState start update) Wire.clockSession

foreign export ccall sdlMain :: IO ()


------------------------------------------------------------------------------
data Game = Game
    { gplayer    :: Sprite
    , background :: Sprite
    , garrows    :: Direction
    }

player :: Lens' Game Sprite
player f game = fmap (\g -> game { gplayer = g }) $ f (gplayer game)

arrows :: Lens' Game Direction
arrows f game = fmap (\a -> game { garrows = a }) $ f (garrows game)


------------------------------------------------------------------------------
eventLoop :: Surface -> Wire () IO Event [Sprite] -> Session IO -> IO ()
eventLoop screen wire session = do
    event <- SDL.pollEvent
    (result, wire', session') <- Wire.stepSession wire session event
    flip (either return) result $ \sprites -> do
        render screen sprites
        eventLoop screen wire' session'


------------------------------------------------------------------------------
render :: Surface -> [Sprite] -> IO ()
render screen sprites = do
    void $ SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0 0 0 >>=
        SDL.fillRect screen Nothing
    mapM_ (Sprite.draw screen) sprites
    SDL.flip screen


------------------------------------------------------------------------------
update :: Time -> (Event, Game) -> (Either () [Sprite], Game)
update dt (event, game) = if Events.isQuit event
    then (Left (), game) else lmap Right $ update' dt event game


------------------------------------------------------------------------------
update' :: Time -> Event -> Game -> ([Sprite], Game)
update' _ event game =
    (sprites, set arrows arrows' . set player player' $ game)
  where
    arrows' = game^.arrows <> Events.direction event
    player' = Sprite.move (Direction.movement 3 arrows') $ game^.player
    sprites = [background game, player']


------------------------------------------------------------------------------
-- | Cabal doesn't seem to pass -no-hs-main to GHC.  This whole function
-- should be removed once this problem is resolved.
main :: IO ()
main = return ()

