import qualified Data.Vector as V
import Control.Lens

angle2 :: V2 Float -> V2 Float -> V2 Float
angle2 a b = acos $ dot a b ^/ sqr (quadrance a * quadrance b)

type Position = V2 Float
type Velocity = V2 Float
type Force = V2 Float

wire :: (MonadUI Graphics m, MonadState World m) => V.Vector (Velocity, Position) -> m ()
wire points = do
    let distrib is = 1 : repeat 0
    let distIn is = zipWith (*) (distrib is) $ concatMap (flip toListOf points . ix) is
    toListOf _2
    tick
    wire $ execStateT ?? points $ forM_ (itoList points) $ \(i, (vel, pos)) -> do
        
        (fl, fr) <- elas (distIn [i - 1, i - 2]) pos (distIn [i + 1, i + 2])

        points . ix (i - 1) . _1 +~ fl
        points . ix i . _1 -~ fl + fr
        points . ix (i + 1) . _1 +~ fr
    where
        elas :: Position -> Position -> Position -> (Force, Force)
        elas l p r = (signorm (perp pl) ^* str, signorm (perp pr) ^* str) where
            k = 0.1
            el = 1
            pl = p - l
            pr = p - r
            str = el / (angle2 pl pr / 2)