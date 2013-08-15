{-# LANGUAGE LambdaCase, TemplateHaskell #-}
import Control.Lens
import Control.Bool
import Graphics.UI.FreeGame
import Control.Monad.State

gravity = 0.2

animationPeriod = 32

loadBitmaps "images"

data World = World
    { _playerCoord :: V2 Float
    , _playerVelocity :: V2 Float
    , _prevKeyUp :: Bool
    , _playerDirection :: Int
    , _playerAnimation :: Int
    , _isMoving :: Bool
    }
makeLenses ''World

runPlayer :: StateT World Game ()
runPlayer = do
    whenM (uses prevKeyUp not <&&> keySpecial KeyUp) $ do
        playerVelocity += V2 0 (-4)
    prevKeyUp <~ keySpecial KeyUp
    whenM (keySpecial KeyLeft) $ playerCoord -= V2 2 0 >> playerDirection .= 1
    whenM (keySpecial KeyRight) $ playerCoord += V2 2 0 >> playerDirection .= 2
    isMoving <~ keySpecial KeyLeft <||> keySpecial KeyRight 
    v <- use playerVelocity
    playerCoord += v
    playerVelocity .= v + V2 0 gravity
    unlessM (uses (playerCoord._y) (<440)) $ playerVelocity . _y .= 0 >> playerCoord . _y .= 440
    bmp <- character <$> uses playerAnimation (`div`(animationPeriod`div`4)) <*> use playerDirection
    use playerCoord >>= flip translate (fromBitmap bmp)
    whenM (use isMoving) $ playerAnimation %= (`mod`animationPeriod) . succ

character 0 r = cropBitmap _Actor_png (32, 32) (0, r * 32)
character 1 r = cropBitmap _Actor_png (32, 32) (32, r * 32)
character 2 r = cropBitmap _Actor_png (32, 32) (64, r * 32)
character 3 r = cropBitmap _Actor_png (32, 32) (32, r * 32)

main = runGame def $ flip evalStateT World { _playerCoord = V2 240 240
        , _playerVelocity = V2 0 0
        , _prevKeyUp = False
        , _playerDirection = 0
        , _playerAnimation = 0
        , _isMoving = False } $ foreverTick $ do
    runPlayer