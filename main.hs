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
import Data.Maybe

gravity = 0.2
friction = 0.1

loadBitmaps "images"

tileCoord :: Iso' (V2 Float) (V2 Int)
tileCoord = mapping $ iso (floor.(/32)) ((*32).fromIntegral)

renderField :: (Picture2D m, Monad m) => Field -> m ()
renderField field = translate (V2 16 16) $ forM_ (V2 <$> [c..c+20] <*> [r..r+15]) $ \t -> translate (t ^. from tileCoord) $ do
    case field ^? chip . ix t of
        Just Goal -> fromBitmap _star_png
        Just Wall -> fromBitmap _tile_png
        _ -> return ()
        where
            V2 c r = field ^. viewPosition . tileCoord

isGoal :: MonadState World m => V2 Float -> m Bool
isGoal pos = liftM (maybe False (==Goal)) $ preuse $ field . chip . ix (view tileCoord pos)

canMove :: MonadState World m => V2 Float -> m Bool
canMove pos = liftM (maybe True (/=Wall)) $ preuse $ field . chip . ix (view tileCoord pos)

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

runPlayer :: (Applicative m, Figure2D m, Keyboard m, MonadState World m) => StateT Player m ()
runPlayer = do
    ppos <- use position
    Sum acc <- execWriterT $ alterMOf playerShots $ \s -> do
        let pos = view position s
        let hold
                | Just k <- view shotWithWire s, view shotPersist s > 0 = do
                    tell $ Sum $ normalize (pos - ppos) ^* qd pos ppos ** 0.25 ^/ 40 ^* k
                    return $ Just $ s & shotPersist -~ 1
                | otherwise = return Nothing

        when (views shotWithWire isJust s) $ colored red $ line [ppos, pos]
        translate pos $ rotateR (view shotRotation s) $ fromBitmap _suriken_png
        b <- lift . lift $ canMove pos
        if b
            then do
                r <- lift . lift $ hurtEnemy 1 pos (views velocity normalize s) 16
                if r
                    then do
                        lift . lift $ effects %= (translate pos effectAttack:)
                        hold
                    else return $ Just $ s & updatePosition & shotRotation +~ 6 & velocity +~ V2 0 (gravity/3) & velocity *~ 0.99
            else hold

    vel <- use velocity
    unlessM (lift $ canMove (ppos + acc * V2 1 0)) $ when (views _x abs acc - view _y acc / 4 >= 0.05) $ do
        whenM (keySpecial KeyUp) $ velocity . _y %= max (-4) . subtract 0.3
    velocity += acc

    whenM (not <$> use holdShift <&&> keySpecial KeyLeftShift) $ do
        xs <- use playerShots
        case unsnoc xs of
            Nothing -> return ()
            Just (rs, l) -> whenM (notM $ lift $ canMove $ view position l) $ playerShots .= rs
    holdShift <~ keySpecial KeyLeftShift

    isLanding <- common $ characterBitmap 32 32 $ cropBitmap _Actor_png (96, 128) (0, 0)

    vel <- use velocity
    ppos <- use position

    whenM (not <$> keyChar 'Z') $ use playerCharge >>= \case
        0 -> return ()
        a -> launch a >> playerCharge .= 0
    whenM (keyChar 'Z') $ do
        whenM (uses playerCharge (>=60)) $ colored red $ translate ppos (circleOutline 16)
        playerCharge += 1
    whenM (keySpecial KeyUp <&&> pure isLanding) $ playerVelocity += V2 0 (-6) 
    whenM (keySpecial KeyLeft) $ velocity . _x %= max (-4) . subtract 0.2 >> direction .= 1
    whenM (keySpecial KeyRight) $ velocity . _x %= min 4 . (+0.2) >> direction .= 2
    whenM (keySpecial KeyDown) $ velocity . _y += 0.2 >> direction .= 0

    when (quadrance vel > 6^2) $ playerBlow .= 10
    b <- use playerBlow
    when (b > 0) $ do
        lift $ hurtEnemy 1 ppos vel 24
        colored (Color 0 0.9 0.9 (fromIntegral b / 10)) $ translate ppos $ circleOutline (48 - fromIntegral b * 2)
        playerBlow -= 1


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
                playerShots %= (Shot pos (vel + normalize vel + vel' + V2 0 (-1)) 0 Nothing 60:)
            | otherwise = do
                pos <- use position
                vel <- use velocity
                d <- use direction
                let vel' = case d of
                        1 -> V2 (-12) 0
                        2 -> V2 12 0
                        _ -> zero
                playerShots %= (Shot pos (vel + normalize vel + vel' + V2 0 (-1)) 0 (Just 1) 300:)

effectAttack :: Free GUI ()
effectAttack = do
    let go c = fromBitmap (cropBitmap _Hit_png (100, 100) c) >> tick
    mapM_ go $ (,) <$> [0,100..700] <*> [0,100]

loadMap :: FilePath -> IO (Field, [Enemy])
loadMap path = do
    let push x = tell (Endo (x:))
    rows <- map words <$> lines <$> readFile path
    let (cs, Endo f) = runWriter $ liftM concat $ forM (zip [0..] rows)
            $ \(r, row) -> forM (zip [0..] row)
            $ \(c, ch) -> let crd = V2 c r in liftM ((,) crd) $ case ch of
                "*" -> return Wall
                "-" -> return EmptyTile
                "!" -> return Goal
                "0" -> push (newEnemy & position . tileCoord .~ crd) >> return EmptyTile
                "1" -> push (newEnemy & position . tileCoord .~ crd
                        & enemyCharacter .~ Fly
                        & enemyHP .~ 5) >> return EmptyTile
    return (Field (M.fromList cs) (V2 0 0), f [])

hurtPlayer :: MonadState World m => V2 Float -> V2 Float -> m Bool
hurtPlayer p force = do
    q <- use (thePlayer . position)
    liftM getAny $ execWriterT $ whenM (uses (thePlayer . invincibleDuration) (==0)) $ do
        when (qd p q < 32) $ do
            thePlayer . playerHP -= 1
            thePlayer . playerInvincible .= 30
            thePlayer . velocity += force
            tell $ Any True

hurtEnemy :: MonadState World m => Int -> V2 Float -> V2 Float -> Float -> m Bool
hurtEnemy damage center force size = liftM getAny $ execWriterT
    $ alterIntMapMOf enemies $ \e -> if qd center (view position e) < size^2 && view invincibleDuration e == 0 
        then do
            tell $ Any True
            return $ Just $ e
                & enemyHP -~ damage & velocity +~ (force - V2 0 0.5) + normalize (view position e - center) ^* norm force
                & enemyAttacked .~ True
                & invincibleDuration .~ 30
        else return (Just e)

runEnemy :: forall m. (MonadState World m, Applicative m, Picture2D m) => Enemy -> m (Maybe Enemy)
runEnemy e = runMaybeT $ flip execStateT e $ do
    uses enemyHP (>0) >>= guard
    cha <- use enemyCharacter
    isLanding <- common $ case cha of
        Squirt -> characterBitmap 32 48 $ cropBitmap _vx_chara08_a_png (96, 192) (192, 192)
        Fly -> characterBitmap 32 48 $ cropBitmap _vx_chara08_a_png (96, 192) (288, 192)
    whenM (uses (velocity . _x) (<0)) $ direction .= 1
    whenM (uses (velocity . _x) (>0)) $ direction .= 2
    m <- use enemyStrategy
    when isLanding $ enemyStrategy <~ exec cha m
    case cha of
        Fly -> modify updateAnimation >> velocity -= V2 0 (gravity * 0.8)
        _ -> return ()
    p <- use position
    v <- use velocity
    d <- use direction
    lift $ hurtPlayer p $ (V2 0 (-2)+) $ case d of
        1 -> V2 (-3) 0
        2 -> V2 3 0
        _ -> V2 0 0
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
        exec _ (Get :>>= cont) = cont <$> get
        exec _ (Put s :>>= cont) = cont <$> put s

common :: (HasAnimationComponent t, HasPosition t, HasVelocity t, HasInvincibleDuration t
    , Applicative m, Picture2D m, MonadState World m, HasHP t) => (Int -> Int -> Bitmap) -> StateT t m Bool
common base = do
    vel <- use velocity
    pos <- use position
    
    when (view _y pos > 480) $ _HP .= 0

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
    bmp <- base <$> uses animation (`div`(animationPeriod`div`4)) <*> use direction
    tr <- ifThenElseM (uses invincibleDuration (>0)) 
        (invincibleDuration -= 1 >> return (colored (Color 1 1 1 0.5)))
        (return id)
    tr $ use position >>= flip translate (fromBitmap bmp)

    return isLanding

characterBitmap w h b 0 r = cropBitmap b (w, h) (0, r * h)
characterBitmap w h b 1 r = cropBitmap b (w, h) (w * 1, r * h)
characterBitmap w h b 2 r = cropBitmap b (w, h) (w * 2, r * h)
characterBitmap w h b 3 r = cropBitmap b (w, h) (w * 3, r * h)

unsafeSight :: Monad m => Lens' s t -> StateT t (StateT s m) a -> StateT s m a
unsafeSight l m = StateT $ \s -> do
    ((a, t'), s') <- m `runStateT` view l s `runStateT` s
    return (a, set l t' s')

scroll :: (Keyboard m, MonadState World m) => m ()
scroll = do
    v <- uses (field . viewPosition) (+ V2 240 240)
    p <- use $ thePlayer . position
    let d = view _x (v - p)
        tr = 48
        tl = 48
    when (d > tr) $ field . viewPosition . _x -= (d - tr)
    when (d < (-tl)) $ field . viewPosition . _x -= (d + tl)

main = do
    (f, es) <- loadMap "map.map"
    runGame def $ flip evalStateT World
        { _thePlayer = newPlayer & position .~ V2 80 240
        , _enemies = IM.fromList (zip [0..] es)
        , _effects = []
        , _field = f
        } $ foreverTick $ do
        translate (V2 320 240) $ fromBitmap _background_png
        
        ppos <- use $ thePlayer . position

        scroll

        offset <- use $ field . viewPosition
        translate (negate offset) $ do
            use field >>= renderField
            unsafeSight thePlayer runPlayer
            
            alterIntMapMOf enemies runEnemy
            
            es <- use effects
            effects <~ fmap (concatMap $ either return (const [])) (mapM untick es)

        whenM (isGoal ppos) $ translate (V2 320 240) $ fromBitmap _clear_png

        hp <- use $ thePlayer . playerHP
        unless (hp > 0) quit

        forM_ [0..hp-1] $ \i -> translate (V2 (fromIntegral i * 32 + 40) 40) $ fromBitmap _heart_png