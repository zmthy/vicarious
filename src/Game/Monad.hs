------------------------------------------------------------------------------
-- | Defines a pure monad for the game's pure core to run in.
module Game.Monad
    (
    -- * Monad
      GameM

    -- * Running
    , runGameM
    ) where

import Game.Prelude
------------------------------------------------------------------------------
import Control.Wire


------------------------------------------------------------------------------
-- | The game monad.  Embeds the necessary state in the game's reactive loop
-- without defaulting to the IO monad.
newtype GameM a = GameM { ungame :: State StdGen a }
    deriving (Functor, Applicative, Monad)

instance MonadRandom GameM where
    getRandom = runRandom random
    getRandomR r = runRandom $ randomR r

runRandom :: Random a => (StdGen -> (a, StdGen)) -> GameM a
runRandom f = GameM $ do
    s <- get
    let (a, s') = f s
    put s'
    return a


------------------------------------------------------------------------------
-- | Runs the game with the given random generator.
runGameM :: StdGen -> GameM a -> (a, StdGen)
runGameM = flip $ runState . ungame

