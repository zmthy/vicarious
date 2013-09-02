------------------------------------------------------------------------------
-- | Main module.  Exposes the SDL main function, which sets up the game loop
-- and pushes events into the Netwire session.
module Main (main, sdlMain) where

import Game.Prelude
------------------------------------------------------------------------------
import qualified Control.Wire as Wire
import qualified Game.Sprite as Sprite
import qualified Graphics.UI.SDL as SDL
------------------------------------------------------------------------------
import Control.Wire (Wire)
import Game.Assets
import Game.Data
import Game.Monad
import Game.Sprite
import Game.Wire
import Graphics.UI.SDL (Event, Surface)


------------------------------------------------------------------------------
-- | The real main function.  Sets up SDL and starts the game.
sdlMain :: IO ()
sdlMain = SDL.withInit [SDL.InitVideo] $ do
    screen <- uncurry SDL.setVideoMode (960, 540) 0 [SDL.HWSurface, SDL.Fullscreen]
    SDL.rawSetCaption (Just "Vicarious") Nothing

    back <- Sprite.load "road/background"
    bgt  <- Sprite.load "road/tape"
    on   <- Sprite.load "road/background/light"
    off  <- Sprite.load "road/background/dark"

    p1 <- Sprite.load "road/paramedic/1"
    pt <- Sprite.load "road/paramedic/2"
    p2 <- Sprite.load "road/paramedic/3"

    let gmove = map . Sprite.move $ scale (134, 72)
    ghost <- gmove $ Sprite.load "road/ghost"
    let gload m a n = forM ([1..n] :: [Int]) $
            m . Sprite.load . (("road/" <> a <> "/") <>) . show

    fadea <- gload gmove "ghost" 8

    let rmove = map . Sprite.move $ scale (134, 37)
    roda  <- gload rmove "get" 6
    casta <- gload rmove "cast" 10
    lossa <- gload rmove "break" 5
    gp <- rmove $ Sprite.load "road/pause"
    gc <- rmove $ Sprite.load "road/in"
    gy <- rmove $ Sprite.load "road/pull"
    gb <- rmove $ Sprite.load "road/break"

    let bg     = Background back on off bgt
        medic  = Paramedic p1 pt p2
        start  = Game ghost DUp 0 0 fishStart
        assets = Assets bg medic $ Ghost fadea roda casta gp gc gy lossa gb

    render screen [bgt, on, p1, back]

    runGameMT start $ eventLoop screen (gameWire assets)
  where scale = twimap (* pixel)

foreign export ccall sdlMain :: IO ()



------------------------------------------------------------------------------
eventLoop :: Surface -> Wire () GameM Event [Sprite] -> GameMT IO ()
eventLoop screen = fix $ \eloop wire -> do
    (result, wire') <- lift SDL.pollEvent >>= tick =>=>
        dot liftGameM (flip $ Wire.stepWire wire)
    flip (either return) result $ \sprites -> do
        lift $ render screen sprites
        eloop wire'


------------------------------------------------------------------------------
-- | Renders the sprites on the surface.
render :: Surface -> [Sprite] -> IO ()
render screen sprites = do
    mapM_ (Sprite.draw screen) $ reverse sprites
    SDL.flip screen


------------------------------------------------------------------------------
-- | GHC doesn't seem to recognise -no-hs-mainThis whole function should be
-- removed once this problem is resolved.
main :: IO ()
main = return ()

