{-# LANGUAGE LambdaCase, TemplateHaskell, GADTs, FlexibleContexts, Rank2Types, ScopedTypeVariables, MultiWayIf #-}
import Control.Lens
import Control.Bool
import Graphics.UI.FreeGame
import Control.Monad.State
import Wire
import Types
import Data.Void
import Control.Monad.Free
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Vector as V
import Control.Monad.Operational.Mini

gravity = 0.15

loadBitmaps "images"

tileCoord :: Iso' (V2 Float) (V2 Int)
tileCoord = mapping $ iso (floor.(/32)) ((*32).fromIntegral)

renderField :: (Picture2D m, Monad m) => Field -> m ()
renderField _ = translate (V2 16 16) $ forM_ (V2 <$> [0..19] <*> [0..14]) $ \c -> translate (c ^. from tileCoord) $ fromBitmap $ cropBitmap _Outside_A5_png (32, 32) (0, 32*6)

castSkill :: MonadState World m => Int -> StateT Player m ()
castSkill _ = do
    pos <- use position
    lift $ effects %= (translate pos effectAttack:)

runPlayer :: (Applicative m, Picture2D m, Keyboard m, MonadState World m) => StateT Player m ()
runPlayer = do
    isLanding <- common $ cropBitmap _Actor_png (96, 128) (0, 0)
    whenM (not <$> keyChar 'Z') $ use playerCharge >>= \case
        0 -> return ()
        a -> castSkill a >> playerCharge .= 0
    whenM (keyChar 'Z') $ playerCharge += 1
    whenM (pure isLanding <&&> keySpecial KeyUp) $ playerVelocity += V2 0 (-4)
    whenM (keySpecial KeyLeft) $ position -= V2 2 0 >> direction .= 1
    whenM (keySpecial KeyRight) $ position += V2 2 0 >> direction .= 2
    whenM (keySpecial KeyLeft <||> keySpecial KeyRight) $ modify updateAnimation

type Strategy = Program Tactic

data Tactic x where
    Approach :: Tactic ()
    Wait :: Tactic ()

defaultStrategy = singleton Approach

effectAttack :: Free GUI ()
effectAttack = do
    let cropped = cropBitmap _Attack5_png (192, 192)
    fromBitmap $ cropped (192 * 0, 0)
    tick
    fromBitmap $ cropped (192 * 1, 0)
    tick
    fromBitmap $ cropped (192 * 2, 0)
    tick
    fromBitmap $ cropped (192 * 3, 0)
    tick
    fromBitmap $ cropped (192 * 4, 0)
    tick
    fromBitmap $ cropped (192 * 0, 192)
    tick
    fromBitmap $ cropped (192 * 1, 192)
    tick

friction :: MonadState World m => V2 Float -> m Float
friction _ = return 0

runEnemy :: forall m. (MonadState World m, Applicative m, Picture2D m)  => Strategy () -> StateT Enemy m ()
runEnemy m = do
    common (cropBitmap _Monster1_png (96, 128) (192, 0))
    whenM (uses (velocity . _x) (<0)) $ direction .= 1
    whenM (uses (velocity . _x) (>0)) $ direction .= 2
    interpret exec m
    modify updateAnimation
    return ()
    where
        exec :: Tactic x -> StateT Enemy m x
        exec Approach = do
            target <- lift $ use $ thePlayer . position
            pos <- use position

            velocity += (normalize (target - pos) & _y .~ 0) ^* 0.1

        exec Wait = return ()
        exec Flee = do
            target <- lift $ use $ thePlayer . position
            pos <- use position

            velocity -= (normalize (target - pos) & _y .~ 0) ^* 0.1

common :: (HasAnimationComponent t, HasPosition t, HasVelocity t
    , Applicative m, Picture2D m, MonadState World m) => Bitmap -> StateT t m Bool
common base = do
    modify updatePosition
    isLanding <- uses (position._y) (>=440)
    if isLanding
        then velocity . _y .= 0 >> position . _y .= 440
        else velocity += V2 0 gravity
    bmp <- character base <$> uses animation (`div`(animationPeriod`div`4)) <*> use direction
    use position >>= flip translate (fromBitmap bmp)
    return isLanding

character b 0 r = cropBitmap b (32, 32) (0, r * 32)
character b 1 r = cropBitmap b (32, 32) (32, r * 32)
character b 2 r = cropBitmap b (32, 32) (64, r * 32)
character b 3 r = cropBitmap b (32, 32) (32, r * 32)

unsafeSight :: Monad m => Lens' s t -> StateT t (StateT s m) a -> StateT s m a
unsafeSight l m = StateT $ \s -> do
    ((a, t'), s') <- m `runStateT` view l s `runStateT` s
    return (a, set l t' s')

main = runGame def $ flip evalStateT World { _thePlayer = newPlayer, _enemies = IM.singleton 0 newEnemy
    , _effects = [wire $ V.fromList $ map ((,) zero) [V2 40 40, V2 140 140, V2 180 120, V2 240 100, V2 280 80, V2 300 40]] } $ foreverTick $ do
    use field >>= renderField
    unsafeSight thePlayer runPlayer
    n <- uses enemies IM.size
    forM_ [0..n-1] $ \i -> do
        unsafeSight (unsafeSingular $ enemies . ix i) $ runEnemy defaultStrategy 
    es <- use effects
    effects <~ fmap (concatMap $ either return (const [])) (mapM untick es)