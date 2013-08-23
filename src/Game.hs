------------------------------------------------------------------------------
-- | Main module.  Exposes the SDL main function, which sets up the game loop
-- and pushes events into the Netwire session.
module Main (main, sdlMain) where

import Game.Prelude
------------------------------------------------------------------------------
import qualified Game.Sprite as Sprite
import qualified Graphics.UI.SDL as SDL


------------------------------------------------------------------------------
-- | The real main function.  Sets up SDL and starts the game.
sdlMain :: IO ()
sdlMain = SDL.withInit [SDL.InitVideo] $ do
    screen <- SDL.setVideoMode 800 600 0 [SDL.SWSurface]
    SDL.rawSetCaption (Just "Vicarious") Nothing
    void $ SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0 0 0 >>=
        SDL.fillRect screen Nothing
    ghost <- Sprite.load "ghost"
    Sprite.draw screen ghost
    SDL.flip screen
    fix events

foreign export ccall sdlMain :: IO ()


------------------------------------------------------------------------------
events :: IO () -> IO ()
events self = do
    event <- SDL.pollEvent
    case event of
        SDL.KeyUp k -> flip unless self $ SDL.symKey k == SDL.SDLK_q &&
            SDL.KeyModLeftMeta `elem` SDL.symModifiers k
        SDL.Quit    -> return ()
        _           -> self


------------------------------------------------------------------------------
-- | Cabal doesn't seem to pass -no-hs-main to GHC.  This whole function
-- should be removed once this problem is resolved.
main :: IO ()
main = return ()

