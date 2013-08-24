------------------------------------------------------------------------------
-- | A better prelude.
module Game.Prelude
    (
      -- * New modules
      module Prelude

      -- * Booleans
    , iff
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

      -- * Nested application
    , (<<$>>)
    , (<<*>>)
    , (<<**>>)

      -- * Debug
    , traceId
    , traceM
    , traceShowM
    ) where

------------------------------------------------------------------------------
import Control.Applicative as Prelude
import Control.Arrow as Prelude
import Control.Category as Prelude
import Control.Lens as Prelude hiding (lmap, rmap, (<~))
import Control.Monad as Prelude
import Control.Monad.State as Prelude
import Data.Bifunctor.Apply as Prelude hiding (first, second, (<<$>>))
import Data.Either as Prelude
import Data.Foldable as Prelude (concat, elem, notElem, foldl, foldr)
import Data.Functor.Contravariant as Prelude
import Data.Maybe as Prelude
import Data.Monoid as Prelude
import Debug.Trace as Prelude
import Prelude
    hiding (concat, elem, notElem, foldl, foldr, id, map, (++), (.))


------------------------------------------------------------------------------
-- | Function form of if-then-else.
iff :: Bool -> a -> a -> a
iff c t e = if c then t else e


------------------------------------------------------------------------------
-- | Variant of 'iff' with the condition at the end.
(?) :: a -> a -> Bool -> a
(?) = dot flip flip iff

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
map = fmap


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
-- | Nested Functor map.
mapmap :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
mapmap = map . map


------------------------------------------------------------------------------
-- | Operator synonym for 'mapmap'.
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = mapmap

infixl 4 <<$>>


------------------------------------------------------------------------------
-- | Nested Applicative application.
(<<*>>) :: (Applicative f, Applicative g)
      => f (g (a -> b)) -> f (g a) -> f (g b)
(<<*>>) = (<*>) . map (<*>)

infixl 4 <<*>>


------------------------------------------------------------------------------
(<<**>>) :: (Applicative f, Applicative g)
       => f (g a) -> f (g (a -> b)) -> f (g b)
(<<**>>) = flip (<<*>>)

infixl 4 <<**>>


------------------------------------------------------------------------------
-- | Shows the value and evaluates to it.
traceId :: Show a => a -> a
traceId = join traceShow


------------------------------------------------------------------------------
-- | Monadic trace.
traceM :: Monad m => String -> m ()
traceM s = trace s $ return ()


------------------------------------------------------------------------------
-- | Monadic traceShow.
traceShowM :: (Show a, Monad m) => a -> m ()
traceShowM a = traceShow a $ return ()

