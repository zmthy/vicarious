------------------------------------------------------------------------------
-- | Defines the game animations as wires.
module Game.Wire
    (
      gameWire
    ) where

import Game.Prelude hiding (until)
------------------------------------------------------------------------------
import qualified Game.Events as Events
import qualified Game.Sprite as Sprite
import qualified Graphics.UI.SDL as SDL
------------------------------------------------------------------------------
import Control.Wire
import Game.Assets
import Game.Data
import Game.Monad
import Game.Sprite


------------------------------------------------------------------------------
gameWire :: Assets -> Wire () GameM SDL.Event [Sprite]
gameWire (Assets bg p ghost) = until Events.isQuit >>>
    (\s ss -> tape bg : maybeToList s <> ss) <$> ghostWire ghost <*> flatten
        [ shadowWire (light bg) (dark bg)
        , paramedicWire p
        , pure $ static bg
        ]
  where
    flatten = flip foldr (pure []) $ (<*>) . (<$>) (:)


------------------------------------------------------------------------------
ghostWire :: Ghost -> Wire () GameM SDL.Event (Maybe Sprite)
ghostWire ghost = (pure Nothing . until Events.isSpace -->) . map Just $
        wireFrames 0.2 (fade ghost)
    --> chill . for 2
    --> hold_ flyUp
    <|> chill . untilM (const $ (<= 37 * pixel) <$> use (player.position._2))
    --> chill . until Events.isSpace
    --> wireFrames 0.05 (rod ghost)
    --> gsprite pause . for 1
    --> reGhost
  where
    effect = perform `dot` pure
    look = effect . use
    place l = perform . arr (l .=) &&& id >>> arr snd
    chill = look player
    flyUp = place player . arr (Sprite.move (0, -pixel)) . look player . periodically 0.1
    gsprite f = pure $ f ghost
    reGhost = wireForever $
            wireFrames 0.05 (cast ghost)
        --> wireForever (fishWire ghost) . untilM (const $ (<= 0) <$> use fish)
        --> wireFrames 0.05 (loss ghost)
        --> gsprite done . effect (fish .= fishStart) . until Events.isSpace


------------------------------------------------------------------------------
fishWire :: Ghost -> Wire () GameM SDL.Event Sprite
fishWire ghost = fishAnim .  perform . arr respond
  where
    respond e = flip (maybe $ return ()) (Events.mouseMove e) $
        \(_, _, dx, dy) -> do
            (d, l, c, f) <- directionTick dx dy <$> get
            if f d <= c then l += d else do
                direction %= succ
                l .= 0
                fish += 12
            fish %= dropFish
    flift f = pure $ f ghost
    fishAnim = flift cast' . wackelkontaktM 0.9 --> flift yank . for 0.1
    directionTick dx dy game = let dir = game^.direction in if dir `elem` [DUp, DDown]
        then (dy + game^.vertical, vertical, 50, sign dir)
        else (dx + game^.horizontal, horizontal, 20, sign dir)
    sign dir = if dir `elem` [DUp, DLeft] then negate else id
    dropFish fi = if fi > 10 then fi `div` 2 else fi - 1


------------------------------------------------------------------------------
shadowWire :: Sprite -> Sprite -> Wire () GameM a Sprite
shadowWire on off = wireForever $
        pure on . forRM (15, 25)
    --> wireForever (decisecond off --> decisecond on) . forRM (0.1, 0.6)
  where decisecond = (for 0.1 >>>) . pure


------------------------------------------------------------------------------
paramedicWire :: Paramedic -> Wire () GameM a Sprite
paramedicWire Paramedic { para1 = p1, paraT = pt, para2 = p2 } =
    wireForever $ thenTransition p1 --> thenTransition p2
  where thenTransition p = pure p . forRM (8, 12) --> pure pt . for 1


------------------------------------------------------------------------------
-- | Produces each sprite for the given amount of time, then inhibits.
wireFrames :: Time -> [Sprite] -> Wire () GameM a Sprite
wireFrames _ []       = empty
wireFrames t (s : ss) =
    (fromJust <$> until isNothing <<<) . hold (Just s) $
    (Just <$> list ss --> pure Nothing) . periodically t


------------------------------------------------------------------------------
-- | Causes the wire to restart when it inhibits.
wireForever :: Monad m => Wire e m a b -> Wire e m a b
wireForever = fix . (-->)


------------------------------------------------------------------------------
-- | Produces for a random amount within the given range, then inhibits.
forRM :: (MonadRandom m, Monoid e) => (Time, Time) -> Wire e m a a
forRM r = id &&& noiseRM . pure r >>> mkState 0 stf
  where
    stf dt ((input, endTime), et) = let et' = et + dt in
        (if et' >= endTime then Left mempty else Right input, et')


------------------------------------------------------------------------------
-- | Variant of 'until' that uses the underlying monad to determine when to
-- inhibit.
untilM :: (Monad m, Monoid e) => (a -> m Bool) -> Event e m a
untilM f = mkFixM . const $ \a -> liftM (Left mempty ? Right a) (f a)

