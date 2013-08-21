{-# LANGUAGE LambdaCase, TemplateHaskell, GADTs, FlexibleContexts, Rank2Types #-}
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

animationPeriod = 32

loadBitmaps "images"

renderField :: Monad m => Field -> m ()
renderField _ = return ()

runPlayer :: (Applicative m, Picture2D m, Keyboard m, MonadIO m, MonadState World m) => StateT Player m ()
runPlayer = do
    isLanding <- common $ cropBitmap _Actor_png (96, 128) (0, 0)
    whenM (pure isLanding <&&> keySpecial KeyUp) $ playerVelocity += V2 0 (-4)
    whenM (keySpecial KeyLeft) $ position -= V2 2 0 >> playerDirection .= 1
    whenM (keySpecial KeyRight) $ position += V2 2 0 >> playerDirection .= 2
    whenM (keySpecial KeyLeft <||> keySpecial KeyRight) $ playerAnimation %= (`mod`animationPeriod) . succ

type Strategy = Program Tactic

data Tactic x where
    Approach :: Tactic ()
    Wait :: Tactic ()

defaultStrategy = singleton Approach

runEnemy :: (MonadState World m, Applicative m, Picture2D m, Keyboard m)  => Strategy () -> StateT Enemy m ()
runEnemy m = do
    common (cropBitmap _Monster1_png (96, 128) (192, 0))
    return ()

common :: (HasAnimationComponent t, HasPosition t, HasVelocity t
    , Applicative m, Picture2D m, Keyboard m, MonadState World m) => Bitmap -> StateT t m Bool
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
    , _effects = [wire $ V.fromList $ map ((,) zero) [V2 40 40, V2 140 140, V2 180 140]] } $ foreverTick $ do
    unsafeSight thePlayer runPlayer
    n <- uses enemies IM.size
    forM_ [0..n-1] $ \i -> do
        unsafeSight (unsafeSingular $ enemies . ix i) $ runEnemy defaultStrategy 
    es <- use effects
    effects <~ mapM untickInfinite es