{-# LANGUAGE TemplateHaskell, GADTs, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses #-}
module Types where

import Graphics.UI.FreeGame
import Control.Lens
import Control.Monad.Free
import qualified Data.Map as Map
import Data.Void
import qualified Data.IntMap as IM
import qualified Data.Vector as V
import Control.Monad.Operational.Mini
import Control.Monad.State
import Data.Function
import System.Random

class HasPosition t where
    position :: Lens' t (V2 Float)

class HasVelocity t where
    velocity :: Lens' t (V2 Float)

class HasAnimationComponent t where
    direction :: Lens' t Int
    animation :: Lens' t Int

class HasInvincibleDuration t where
    invincibleDuration :: Lens' t Int

class HasHP t where
    _HP :: Lens' t Int

data Shot = Shot
    { _shotPosition :: V2 Float
    , _shotVelocity :: V2 Float
    , _shotRotation :: Float
    , _shotWithWire :: Maybe Float
    , _shotPersist :: Int
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
    , _playerBlow :: Int
    , _holdShift :: Bool
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
    , _enemyInvincible :: Int
    , _enemyAttacked :: Bool
    }

type Strategy = ReifiedProgram Tactic

instance MonadState Enemy Strategy where
    get = singleton Get
    put s = singleton (Put s)

data Tactic x where
    Approach :: Tactic ()
    Wait :: Tactic ()
    RelativePlayer :: Tactic (Maybe (V2 Float))
    Put :: Enemy -> Tactic ()
    Get :: Tactic Enemy
    Randomly :: Random r => (r, r) -> Tactic r

makeLenses ''Enemy

defaultStrategy :: Strategy ()
defaultStrategy = do
    r <- singleton RelativePlayer
    a <- use enemyAttacked
    case r of
        Just d
            | a -> attacking defaultStrategy
            | quadrance d < 160 ^ 2 -> attacking defaultStrategy
            | quadrance d > 320 ^ 2 -> enemyAttacked .= False
        _ -> singleton Wait

attacking :: Strategy a -> Strategy a
attacking cont = do
    replicateM_ 29 $ singleton Approach
    replicateM_ 33 $ singleton Wait
    cont

newPlayer = Player
    { _playerCoord = V2 240 240
    , _playerVelocity = V2 0 0
    , _playerDirection = 0
    , _playerAnimation = 0
    , _playerCharge = 0
    , _playerShots = []
    , _playerInvincible = 0
    , _playerHP = 10
    , _playerBlow = 0
    , _holdShift = False
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
    , _enemyInvincible = 0
    , _enemyAttacked = False
    }

data Tile = EmptyTile | Goal | Wall deriving (Show, Eq, Ord)

data Field = Field
    { _chip :: Map.Map (V2 Int) Tile
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

instance HasInvincibleDuration Player where
    invincibleDuration = playerInvincible

instance HasPosition Enemy where
    position = enemyCoord

instance HasVelocity Enemy where
    velocity = enemyVelocity

instance HasAnimationComponent Enemy where
    animation = enemyAnimation
    direction = enemyDirection

instance HasHP Enemy where
    _HP = enemyHP
instance HasHP Player where
    _HP = playerHP

instance HasInvincibleDuration Enemy where
    invincibleDuration = enemyInvincible

updatePosition :: (HasPosition t, HasVelocity t) => t -> t
updatePosition t = t & position +~ view velocity t

animationPeriod = 32

updateAnimation :: HasAnimationComponent t => t -> t
updateAnimation = over animation $ (`mod`animationPeriod) . succ
