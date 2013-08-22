{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, LambdaCase #-}
module Wire where
import qualified Data.Vector as V
import Control.Lens
import Graphics.UI.FreeGame
import Control.Monad.State
import Control.Monad.Free.Class

angle2 :: V2 Float -> V2 Float -> Float
angle2 a b = acos $ dot a b / sqrt (quadrance a * quadrance b)

type Position = V2 Float
type Velocity = V2 Float
type Force = V2 Float

wire :: (Figure2D m, MonadFree GUI m, Monad m) => V.Vector (Velocity, Position) -> m a
wire points' = do
    colored red $ line $ toListOf (traverse . _2) points'
    tick
    let points = points' & traverse . _1 *~ 0.98
    (>>=wire) $ execStateT ?? points $ forM_ (itoList points) $ \(i, (vel, pos)) -> do
        case force <$> preview (ix (i - 1) . _2) points <*> pure pos <*> preview (ix (i + 1) . _2) points of
            Just (fl, fr) -> do
                case preview (ix (i - 1) . _2) points of
                    Just posl -> colored blue $ line [posl, posl + fl ^* 20]
                    Nothing -> return ()
                ix (i - 1) . _1 += fl

                colored blue $ line [pos, pos + (fl + fr) ^* 20]
                ix i . _1 -= fl + fr

                case preview (ix (i + 1) . _2) points of
                    Just posr -> colored blue $ line [posr, posr + fr ^* 20]
                    Nothing -> return ()
                ix (i + 1) . _1 += fr
            Nothing -> return ()
        ix i . _2 += vel
    where
        righting :: Position -> Position -> Position -> (Force, Force)
        righting l p r = (signorm (perp pl) ^* str ^/ norm pl, signorm (negate $ perp pr) ^* str ^/ norm pr) where
            el = 30
            pl = p - l
            pr = p - r
            str = sqrt (el / angle2 pl pr - el / pi) * signum (dot (perp pl) pr)
        shrink :: Position -> Position -> Position -> (Force, Force)
        shrink l p r = (pl * sh, pr * sh) where
            sh = 0.005
            pl = p - l - normalize (p - l) * 48
            pr = p - r - normalize (p - r) * 48
        force :: Position -> Position -> Position -> (Force, Force)
        force l p r = do
            let (fle :: V2 Float, fre :: V2 Float) = righting l p r
            let (fls :: V2 Float, frs :: V2 Float) = shrink l p r
            (fle ^+^ fls, fre ^+^ frs)