{-# LANGUAGE TemplateHaskell, GADTs #-}
module Types where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad.Free
import qualified Data.Map as Map
import Data.Void
import qualified Data.IntMap as IM
import qualified Data.Vector as V
import Control.Monad.Operational.Mini

type Strategy = ReifiedProgram Tactic

data Tactic x where
    Approach :: Tactic ()
    Wait :: Tactic ()
    Flee :: Tactic ()
    Attack :: Tactic ()
    RelativePlayer :: Tactic (Maybe (V2 Float))

defaultStrategy :: Strategy ()
defaultStrategy = do
    r <- singleton RelativePlayer
    case r of
        Just d
            | quadrance d < 160 ^ 2 -> do
                replicateM_ 29 $ singleton Approach
                replicateM_ 33 $ singleton Wait
        _ -> singleton Wait

class HasPosition t where
    position :: Lens' t (V2 Float)

class HasVelocity t where
    velocity :: Lens' t (V2 Float)

class HasAnimationComponent t where
    direction :: Lens' t Int
    animation :: Lens' t Int

data Shot = Shot
    { _shotPosition :: V2 Float
    , _shotVelocity :: V2 Float
    , _shotRotation :: Float
    }
makeLenses ''Shot

instance HasVelocity Shot where
    velocity = shotVelocity

instance HasPosition Shot where
    position = shotPosition

data Player = Player
    { _playerCoord :: V2 Float
    , _playerHP :: Int
    , _playerVelocity :: V2 Float
    , _playerDirection :: Int
    , _playerAnimation :: Int
    , _playerCharge :: Int
    , _playerShots :: [Shot]
    , _playerInvincible :: Int
    }
makeLenses ''Player

data EnemyCharacter = Squirt | Fly

data Enemy = Enemy
    { _enemyCoord :: V2 Float
    , _enemyVelocity :: V2 Float
    , _enemyHP :: Int
    , _enemyDirection :: Int
    , _enemyAnimation :: Int
    , _enemyCharacter :: EnemyCharacter
    , _enemyStrategy :: Strategy Void
    }
makeLenses ''Enemy

newPlayer = Player
    { _playerCoord = V2 240 240
    , _playerVelocity = V2 0 0
    , _playerDirection = 0
    , _playerAnimation = 0
    , _playerCharge = 0
    , _playerShots = []
    , _playerInvincible = 0
    }

newEnemy :: Enemy
newEnemy = Enemy
    { _enemyCoord = V2 240 240
    , _enemyVelocity = V2 0 0
    , _enemyDirection = 0
    , _enemyAnimation = 0
    , _enemyHP = 10
    , _enemyCharacter = Squirt
    , _enemyStrategy = forever defaultStrategy
    }

data Field = Field
    { _chip :: Map.Map (V2 Int) Bool
    , _viewPosition :: V2 Float
    }
makeLenses ''Field

data World = World
    { _thePlayer :: Player
    , _enemies :: IM.IntMap Enemy
    , _field :: Field
    , _effects :: [Free GUI ()]
    }
makeLenses ''World

instance HasPosition Player where
    position = playerCoord

instance HasVelocity Player where
    velocity = playerVelocity

instance HasAnimationComponent Player where
    animation = playerAnimation
    direction = playerDirection

instance HasPosition Enemy where
    position = enemyCoord

instance HasVelocity Enemy where
    velocity = enemyVelocity

instance HasAnimationComponent Enemy where
    animation = enemyAnimation
    direction = enemyDirection

updatePosition :: (HasPosition t, HasVelocity t) => t -> t
updatePosition t = t & position +~ view velocity t

animationPeriod = 32

updateAnimation :: HasAnimationComponent t => t -> t
updateAnimation = over animation $ (`mod`animationPeriod) . succ
