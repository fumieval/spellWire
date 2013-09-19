{-# LANGUAGE LambdaCase, TemplateHaskell, GADTs, FlexibleContexts, Rank2Types, ScopedTypeVariables, MultiWayIf #-}
import Control.Lens
import Control.Bool
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import Control.Monad.Operational.Mini
import Control.Monad.Free
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Void
import Data.Monoid
import Graphics.UI.FreeGame
import qualified Data.Traversable
import Types

gravity = 0.2
friction = 0.1

loadBitmaps "images"

tileCoord :: Iso' (V2 Float) (V2 Int)
tileCoord = mapping $ iso (floor.(/32)) ((*32).fromIntegral)

renderField :: (Picture2D m, Monad m) => Field -> m ()
renderField field = translate (V2 16 16) $ forM_ (V2 <$> [c..c+20] <*> [r..r+15]) $ \t -> translate (t ^. from tileCoord) $ do
    case field ^? chip . ix t of
        Just False -> fromBitmap $ _tile_png
        _ -> return ()
        where
            V2 c r = field ^. viewPosition . tileCoord

canMove :: MonadState World m => V2 Float -> m Bool
canMove pos = liftM (maybe False id) $ preuse $ field . chip . ix (view tileCoord pos)

alterMOf :: MonadState s m => Lens' s [a] -> (a -> m (Maybe a)) -> m ()
alterMOf l f = do
    xs <- use l
    mxs <- forM xs (liftM (maybe [] return) . f)
    l .= concat mxs

alterIntMapMOf :: MonadState s m => Lens' s (IM.IntMap a) -> (a -> m (Maybe a)) -> m ()
alterIntMapMOf l f = do
    xs <- uses l IM.toList
    mxs <- forM xs $ \(i, a) -> liftM (maybe [] (return . (,) i)) (f a)
    l .= IM.fromList (concat mxs)

hurtEnemy :: MonadState World m => Int -> V2 Float -> V2 Float -> Float -> m Bool
hurtEnemy damage center force size = liftM getAny $ execWriterT
    $ alterIntMapMOf enemies $ \e -> if qd center (view position e) < size^2
        then do
            tell $ Any True
            return $ Just $ e & enemyHP -~ damage & velocity +~ (force - V2 0 0.5)
        else return (Just e)

runPlayer :: (Applicative m, Picture2D m, Keyboard m, MonadState World m) => StateT Player m ()
runPlayer = do
    alterMOf playerShots $ \s -> do
        let pos = view position s
        b <- lift $ canMove pos
        if b
            then do
                translate pos $ rotateR (view shotRotation s) $ fromBitmap _suriken_png
                r <- lift $ hurtEnemy 1 pos (views velocity normalize s) 16
                if r
                    then do
                        lift $ effects %= (translate pos effectAttack:)
                        return Nothing
                    else return $ Just $ s & updatePosition & shotRotation +~ 6 & velocity +~ V2 0 (gravity/3)
            else
                return Nothing

    isLanding <- common $ cropBitmap _Actor_png (96, 128) (0, 0)
    whenM (not <$> keyChar 'Z') $ use playerCharge >>= \case
        0 -> return ()
        a -> launch a >> playerCharge .= 0
    whenM (keyChar 'Z') $ playerCharge += 1
    when isLanding $ do
        whenM (keySpecial KeyUp) $ playerVelocity += V2 0 (-6)
    whenM (keySpecial KeyLeft) $ velocity . _x %= max (-4) . subtract 0.2 >> direction .= 1
    whenM (keySpecial KeyRight) $ velocity . _x %= min 4 . (+0.2) >> direction .= 2
    whenM (keySpecial KeyDown) $ velocity . _y += 0.2 >> direction .= 0
    ifThenElseM (keySpecial KeyLeft <||> keySpecial KeyRight)
        (modify updateAnimation)
        (bool (velocity . _x *= 0.98) (velocity . _x .= 0) isLanding)
    where
        launch l
            | l < 60 = do
                pos <- use position
                vel <- use velocity
                d <- use direction
                let vel' = case d of
                        1 -> V2 (-8) 0
                        2 -> V2 8 0
                        _ -> zero
                playerShots %= (Shot pos (vel + normalize vel + vel' + V2 0 (-1)) 0:)
            | otherwise = do
                pos <- use position
                vel <- use velocity
                d <- use direction
                let vel' = case d of
                        1 -> V2 (-12) 0
                        2 -> V2 12 0
                        _ -> zero
                playerShots %= (Shot pos (vel + normalize vel + vel' + V2 0 (-1)) 0:)

effectAttack :: Free GUI ()
effectAttack = do
    let go c = fromBitmap (cropBitmap _Attack5_png (192, 192) c) >> tick

    go (192 * 0, 0)
    go (192 * 1, 0)
    go (192 * 2, 0)
    go (192 * 3, 0)
    go (192 * 4, 0)
    go (192 * 0, 192)
    go (192 * 1, 192)

loadMap :: FilePath -> IO (Field, [Enemy])
loadMap path = do
    let push x = tell (Endo (x:))
    rows <- map words <$> lines <$> readFile path
    let (cs, Endo f) = runWriter $ liftM concat $ forM (zip [0..] rows)
            $ \(r, row) -> forM (zip [0..] row)
            $ \(c, ch) -> let crd = V2 c r in liftM ((,) crd) $ case ch of
                "*" -> return False
                "-" -> return True
                "0" -> push (newEnemy & position . tileCoord .~ crd) >> return True
                "1" -> push (newEnemy & position . tileCoord .~ crd & enemyCharacter .~ Fly) >> return True
    return (Field (M.fromList cs) (V2 0 0), f [])

runEnemy :: forall m. (MonadState World m, Applicative m, Picture2D m) => Enemy -> m (Maybe Enemy)
runEnemy e = runMaybeT $ flip execStateT e $ do
    uses enemyHP (>0) >>= guard
    cha <- use enemyCharacter
    isLanding <- common $ case cha of
        Squirt -> cropBitmap _Monster1_png (96, 128) (192, 0)
        Fly -> cropBitmap _Monster1_png (96, 128) (288, 0)
    whenM (uses (velocity . _x) (<0)) $ direction .= 1
    whenM (uses (velocity . _x) (>0)) $ direction .= 2
    m <- use enemyStrategy
    when isLanding $ enemyStrategy <~ exec cha m
    case cha of
        Fly -> modify updateAnimation >> velocity -= V2 0 (gravity * 0.8)
        _ -> return ()
    p <- use position
    lift $ do
        q <- use (thePlayer . position)
        when (qd p q < 32) $ do
            thePlayer . playerHP -= 1
            thePlayer . playerInvincible += 30


    return ()
    where
        exec cha (Approach :>>= cont) = do
            target <- lift $ use $ thePlayer . position
            pos <- use position

            case cha of
                Squirt -> do
                    velocity . _x .= signum (view _x target - view _x pos)
                    modify updateAnimation
                Fly -> velocity .= normalize (target - pos) * V2 3 6
            return (cont ())
 
        exec cha (Wait :>>= cont) = do
            velocity . _x .= 0
            return (cont ())
        exec _ (RelativePlayer :>>= cont) = do
            target <- lift $ use $ thePlayer . position
            pos <- use position
            return (cont (Just $ target - pos))

common :: (HasAnimationComponent t, HasPosition t, HasVelocity t
    , Applicative m, Picture2D m, MonadState World m) => Bitmap -> StateT t m Bool
common base = do
    vel <- use velocity
    pos <- use position
    
    let avoidSink = whenM (use position >>= \p -> lift (notF (canMove (p + V2 0 15)) <&&> canMove (p - V2 0 16)))
            $ position -= V2 0 1 >> avoidSink
    
    tx <- ifThenElseM (lift $ canMove (pos + vel * V2 1 0)) (return $ vel ^. _x) (velocity . _x *= (-0.2) >> return 0)
    ty <- ifThenElseM (lift $ canMove (pos + vel * V2 0 1)) (return $ vel ^. _y) (velocity . _y *= (-0.2) >> return 0)

    position += V2 tx ty
    () <- avoidSink
    isLanding <- notM $ lift $ canMove (pos + V2 0 16 + V2 tx ty)
    if isLanding
        then velocity . _y .= 0
        else velocity += V2 0 gravity >> velocity *= 0.99
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
    whenM (keyChar 'A') $ field . viewPosition -= V2 4 0
    whenM (keyChar 'S') $ field . viewPosition += V2 4 0

main = do
    (f, es) <- loadMap "map.map"
    runGame def $ flip evalStateT World { _thePlayer = newPlayer & position .~ V2 80 240
        , _enemies = IM.fromList (zip [0..] es)
        , _effects = [], _field = f } $ foreverTick $ do
        translate (V2 320 240) $ scale 1.5 $ fromBitmap _Mountains4_png
        
        scroll

        offset <- use $ field . viewPosition
        translate (negate offset) $ do
            use field >>= renderField
            unsafeSight thePlayer runPlayer
            
            alterIntMapMOf enemies runEnemy
            
            es <- use effects
            effects <~ fmap (concatMap $ either return (const [])) (mapM untick es)