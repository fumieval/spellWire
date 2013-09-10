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

gravity = 0.2

loadBitmaps "images"

tileCoord :: Iso' (V2 Float) (V2 Int)
tileCoord = mapping $ iso (floor.(/32)) ((*32).fromIntegral)

renderField :: (Picture2D m, Monad m) => Field -> m ()
renderField field = translate (V2 16 16) $ forM_ (V2 <$> [c..c+20] <*> [r..r+15]) $ \t -> translate (t ^. from tileCoord) $ do
    case field ^? chip . ix t of
        Just False -> fromBitmap $ cropBitmap _Outside_A5_png (32, 32) (0, 32*6)
        _ -> return ()
        where
            V2 c r = field ^. viewPosition . tileCoord

castSkill :: MonadState World m => Int -> StateT Player m ()
castSkill _ = do
    pos <- use position
    lift $ do
        effects %= (translate pos effectAttack:)
        n <- uses enemies IM.size
        w <- use id
        w' <- flip execStateT w $ forM_ [0..n-1] $ \i -> unsafeSight (unsafeSingular $ enemies . ix i) $ do
            p <- use position
            when (quadrance (p - pos) < 72^2) $ velocity += normalize (p - pos) ^* 8 + V2 0 (-4)
        id .= w'

canMove :: MonadState World m => V2 Float -> m Bool
canMove pos = liftM (maybe False id) $ preuse $ field . chip . ix (view tileCoord pos)

runPlayer :: (Applicative m, Picture2D m, Keyboard m, MonadState World m) => StateT Player m ()
runPlayer = do
    isLanding <- common $ cropBitmap _Actor_png (96, 128) (0, 0)
    whenM (not <$> keyChar 'Z') $ use playerCharge >>= \case
        0 -> return ()
        a -> castSkill a >> playerCharge .= 0
    whenM (keyChar 'Z') $ playerCharge += 1
    when isLanding $ do
        whenM (keySpecial KeyUp) $ playerVelocity += V2 0 (-5)
    whenM (keySpecial KeyLeft) $ velocity . _x %= max (-4) . subtract 0.2 >> direction .= 1
    whenM (keySpecial KeyRight) $ velocity . _x %= min 4 . (+0.2) >> direction .= 2
    b <- keySpecial KeyLeft <||> keySpecial KeyRight
    if b
        then modify updateAnimation
        else when isLanding $ velocity . _x .= 0

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

loadMap :: FilePath -> IO Field
loadMap path = do
    xss <- map words <$> lines <$> readFile path
    return $ Field (M.fromList [(V2 c r, x /= "*") | (r, xs) <- zip [0..] xss, (c, x) <- zip [0..] xs]) (V2 0 0)

runEnemy :: forall m. (MonadState World m, Applicative m, Picture2D m) => StateT Enemy m ()
runEnemy = do
    isLanding <- common $ cropBitmap _Monster1_png (96, 128) (192, 0)
    whenM (uses (velocity . _x) (<0)) $ direction .= 1
    whenM (uses (velocity . _x) (>0)) $ direction .= 2
    m <- use enemyStrategy
    when isLanding $ enemyStrategy <~ exec m
    modify updateAnimation
    return ()
    where
        exec :: Strategy Void -> StateT Enemy m (Strategy Void)
        exec (Approach :>>= cont) = do
            target <- lift $ use $ thePlayer . position
            pos <- use position

            velocity . _x .= signum (view _x target - view _x pos)
            return (cont ())

        exec (Wait :>>= cont) = do
            velocity . _x .= 0
            return (cont ())
        exec (Flee :>>= cont) = do
            target <- lift $ use $ thePlayer . position
            pos <- use position

            velocity .= (normalize (target - pos) & _y .~ 0) ^* 2
            return (cont ())

common :: (HasAnimationComponent t, HasPosition t, HasVelocity t
    , Applicative m, Picture2D m, MonadState World m) => Bitmap -> StateT t m Bool
common base = do
    pos <- (+) <$> use position <*> use velocity
    pos' <- use position
    b <- lift $ canMove pos
    let avoidSink = whenM (use position >>= \p -> lift (notF (canMove (p + V2 0 15)) <&&> canMove (p - V2 0 32)))
            $ position -= V2 0 1 >> avoidSink
    if b
        then position .= pos
        else do
            velocity .= zero
    () <- avoidSink
    isLanding <- notM $ lift $ canMove (pos' + V2 0 16)
    if isLanding
        then velocity . _y .= 0
        else velocity += V2 0 gravity
    bmp <- characterBitmap base <$> uses animation (`div`(animationPeriod`div`4)) <*> use direction
    use position >>= flip translate (fromBitmap bmp)
    return isLanding

characterBitmap b 0 r = cropBitmap b (32, 32) (0, r * 32)
characterBitmap b 1 r = cropBitmap b (32, 32) (32, r * 32)
characterBitmap b 2 r = cropBitmap b (32, 32) (64, r * 32)
characterBitmap b 3 r = cropBitmap b (32, 32) (32, r * 32)

unsafeSight :: Monad m => Lens' s t -> StateT t (StateT s m) a -> StateT s m a
unsafeSight l m = StateT $ \s -> do
    ((a, t'), s') <- m `runStateT` view l s `runStateT` s
    return (a, set l t' s') 

scroll :: (Keyboard m, MonadState World m) => m ()
scroll = do
    whenM (keyChar 'A') $ field . viewPosition -= V2 2 0
    whenM (keyChar 'S') $ field . viewPosition += V2 2 0

main = do
    f <- loadMap "map.map"
    runGame def $ flip evalStateT World { _thePlayer = newPlayer & position .~ V2 80 240, _enemies = IM.singleton 0 newEnemy
        , _effects = [], _field = f } $ foreverTick $ do
        translate (V2 320 240) $ scale 1.5 $ fromBitmap _Mountains4_png
        
        scroll

        offset <- use $ field . viewPosition
        translate (negate offset) $ do
            use field >>= renderField
            unsafeSight thePlayer runPlayer
            n <- uses enemies IM.size
            forM_ [0..n-1] $ \i -> do
                unsafeSight (unsafeSingular $ enemies . ix i) runEnemy
            
            es <- use effects
            effects <~ fmap (concatMap $ either return (const [])) (mapM untick es)