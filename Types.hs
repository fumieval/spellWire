{-# LANGUAGE TemplateHaskell #-}
module Types where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad.Free
import Data.Map as Map
import Data.Void
import Data.IntMap as IM

class HasPosition t where
    position :: Lens' t (V2 Float)

class HasVelocity t where
    velocity :: Lens' t (V2 Float)

class HasAnimationComponent t where
    direction :: Lens' t Int
    animation :: Lens' t Int

data Player = Player
    { _playerCoord :: V2 Float
    , _playerVelocity :: V2 Float
    , _playerDirection :: Int
    , _playerAnimation :: Int
    }
makeLenses ''Player

data Enemy = Enemy
    { _enemyCoord :: V2 Float
    , _enemyVelocity :: V2 Float
    , _enemyDirection :: Int
    , _enemyAnimation :: Int
    }
makeLenses ''Enemy

newPlayer = Player
    { _playerCoord = V2 240 240
    , _playerVelocity = V2 0 0
    , _playerDirection = 0
    , _playerAnimation = 0
    }

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

newEnemy :: Enemy
newEnemy = Enemy
    { _enemyCoord = V2 240 240
    , _enemyVelocity = V2 0 0
    , _enemyDirection = 0
    , _enemyAnimation = 0
    }

updatePosition :: (HasPosition t, HasVelocity t) => t -> t
updatePosition t = t & position +~ view velocity t

data Field = Field
    { _chip :: Map.Map (V2 Int) Bool
    }

data World = World
    { _thePlayer :: Player
    , _enemies :: IM.IntMap Enemy
    , _field :: Field
    , _effects :: [Free GUI Void]
    }
makeLenses ''World
