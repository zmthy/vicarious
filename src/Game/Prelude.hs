------------------------------------------------------------------------------
-- | A better prelude.
module Game.Prelude
    (
      -- * New modules
      module Prelude

      -- * Booleans
    , ifThenElse
    , (?)

      -- * Composition
    , owl
    , dot
    , swing

      -- * Functors
    , map
    , twimap
    , (<$$>)
    , lmap
    , lcontramap
    , rmap

    -- * Monads
    , (=>>)
    , (=>=>)

      -- * Debug
    , traceId
    , traceF
    , traceM
    , traceShowM
    ) where

------------------------------------------------------------------------------
import qualified Data.Functor as Functor (fmap)
------------------------------------------------------------------------------
import Control.Applicative as Prelude
import Control.Arrow as Prelude
import Control.Category as Prelude
import Control.Lens.Getter as Prelude
import Control.Lens.Lens as Prelude
import Control.Lens.Setter as Prelude
import Control.Lens.Tuple as Prelude
import Control.Monad as Prelude hiding (fmap)
import Control.Monad.State as Prelude hiding (fmap)
import Data.Bifunctor as Prelude hiding (first, second)
import Data.Either as Prelude
import Data.Foldable as Prelude (concat, elem, notElem, foldl, foldr)
import Data.Functor.Contravariant as Prelude
import Data.Int as Prelude
import Data.Maybe as Prelude
import Data.Monoid as Prelude
import Data.Profunctor as Prelude hiding (WrappedArrow (..), lmap, rmap)
import Data.Word as Prelude
import Debug.Trace as Prelude
import Prelude
    hiding (concat, elem, fmap, notElem, foldl, foldr, id, map, (++), (.))


------------------------------------------------------------------------------
-- | Function form of if-then-else.
ifThenElse :: Bool -> a -> a -> a
ifThenElse c t e = if c then t else e


------------------------------------------------------------------------------
-- | Variant of 'ifThenElse' with the condition at the end.
(?) :: a -> a -> Bool -> a
(?) = dot flip flip ifThenElse

infix 1 ?


------------------------------------------------------------------------------
-- | Owl composition.
--
-- > owl f a g b = f a (g b)
owl :: Category cat => (a -> cat b c) -> a -> cat d b -> cat d c
owl = (.) (.)


------------------------------------------------------------------------------
-- | Dot composition.
--
-- > f `dot` g == (f .) . g
dot :: Category cat => cat c d -> (a -> cat b c) -> a -> cat b d
dot = (.) . (.)


------------------------------------------------------------------------------
-- | Swing composition.
swing :: (((a -> b) -> b) -> c -> d) -> c -> a -> d
swing = flip . (. flip id)


------------------------------------------------------------------------------
-- | Functor map.
map :: Functor f => (a -> b) -> f a -> f b
map = Functor.fmap


------------------------------------------------------------------------------
-- | Bifunctor map over both elements.
twimap :: Bifunctor f => (a -> b) -> f a a -> f b b
twimap = join bimap


------------------------------------------------------------------------------
-- | Operator form of twimap.
(<$$>) :: Bifunctor f => (a -> b) -> f a a -> f b b
(<$$>) = twimap

infixl 4 <$$>


------------------------------------------------------------------------------
-- | Bifunctor map over the left element.
lmap :: Bifunctor f => (a -> b) -> f a c -> f b c
lmap = flip bimap id


------------------------------------------------------------------------------
-- | Profunctor map over the left element.
lcontramap :: Profunctor f => (b -> a) -> f a c -> f b c
lcontramap = flip dimap id


------------------------------------------------------------------------------
-- | Maps the right element of a Bifunctor or a Profunctor.
--
-- Note that this assumes that they have a typical implementation of
-- Functor as well.  Using this function is more clear than an ordinary map
-- on multi-functors, particularly bifunctors (such as functions, or pairs).
rmap :: Functor f => (a -> b) -> f a -> f b
rmap = map


------------------------------------------------------------------------------
-- | The "passover" monadic operator that retrieves the required value for the
-- function from an earlier action, passing it over the left argument.
--
-- It can be used in the following manner.
--
-- > ma >>= mb =>> \a -> ...
--
-- The actions are still sequenced from left to right, but the result of the
-- first action is the one passed to the function k.
(=>>) :: Monad m => m b -> (a -> m c) -> a -> m c
(=>>) = (.) . (>>)

infixl 2 =>>


------------------------------------------------------------------------------
-- | A monadic operator that retrieves the required values for the function
-- from two earlier actions.
--
-- It can be used in the following manner.
--
-- > ma >>= mb =>=> \a b -> ...
(=>=>) :: Monad m => m b -> (a -> b -> m c) -> a -> m c
(=>=>) = (.) . (>>=)

infixl 2 =>=>



------------------------------------------------------------------------------
-- | Shows the value and evaluates to it.
traceId :: Show a => a -> a
traceId = join traceShow


------------------------------------------------------------------------------
-- | Traces the result of applying the function to the value, then evaluates
-- to the value.
traceF :: Show b => (a -> b) -> a -> a
traceF f a = traceShow (f a) a


------------------------------------------------------------------------------
-- | Monadic trace.
traceM :: Monad m => String -> m ()
traceM s = trace s $ return ()


------------------------------------------------------------------------------
-- | Monadic traceShow.
traceShowM :: (Show a, Monad m) => a -> m ()
traceShowM a = traceShow a $ return ()

