{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Utils where

import Types
import Linear
import qualified Data.Map as Map
import qualified Data.Vector  as V
import Apecs
import GDK.Types
import Control.Monad
import Data.Maybe ( fromMaybe )
import GDK.Texture

combatPlayerPos :: V2 Float
combatPlayerPos = V2 213 (-360)

combatEnemyPos :: V2 Float
combatEnemyPos = V2 (640 + 1280 / 3) (-360)

playerSpeed, bulletSpeed, enemySpeed, xmin, xmax :: Float
playerSpeed = 250
bulletSpeed = 500
enemySpeed  = 275
xmin = -640
xmax = 640

hitBonus, missPenalty :: Int
hitBonus = 100
missPenalty = 40

playerPos, scorePos :: V2 Float
playerPos = V2 0 0
scorePos  = V2 xmin (-170)

tileSize :: Num a => a
tileSize = 64

tileCount :: Integer
tileCount = 12

wallBottomCount :: Integer
wallBottomCount = 4

wallLeftCount :: Integer
wallLeftCount = 2

wallRightCount :: Integer
wallRightCount = 2

wallBottomLeftElbowCount :: Integer
wallBottomLeftElbowCount = 2

wallBottomRightElbowCount :: Integer
wallBottomRightElbowCount = 2

wallTopCount :: Integer
wallTopCount = 3

roomOffset :: Num a => a
roomOffset = 4 * tileSize

stepPositionFormula :: Float -> Position -> Velocity -> Position
stepPositionFormula dT (Position p) (Velocity v) = Position (p + dT *^ v)

checkBoundaryBoxIntersection :: V2 Float -> BoundaryBox -> V2 Float -> BoundaryBox -> Bool
checkBoundaryBoxIntersection v1 bb1 v2 bb2 = checkBoundaryBoxTopIntersection v1 bb1 v2 bb2 ||
                                            checkBoundaryBoxBottomIntersection v1 bb1 v2 bb2 ||
                                            checkBoundaryBoxLeftIntersection v1 bb1 v2 bb2 ||
                                            checkBoundaryBoxRightIntersection v1 bb1 v2 bb2

-- Note: Sprite positions are centered based on their Position component
checkBoundaryBoxTopIntersection :: V2 Float -> BoundaryBox -> V2 Float -> BoundaryBox -> Bool
checkBoundaryBoxTopIntersection (V2 x1 y1) (BoundaryBox (w1, h1) (box1, boy1)) (V2 x2 y2) (BoundaryBox (w2, h2) (box2, boy2)) =
    bottom1 < top2 && top1 > top2 && right1 > left2 && left1 < right2
    where
        left1 = x1 + fromIntegral box1 - fromIntegral w1/2
        right1 = x1 + fromIntegral box1 + fromIntegral w1/2
        top1  = y1 + fromIntegral boy1 + fromIntegral h1/2
        bottom1 = y1 + fromIntegral boy1 - fromIntegral h1/2
        left2 = x2 + fromIntegral box2 - fromIntegral w2/2
        right2 = x2 + fromIntegral box2 + fromIntegral w2/2
        top2 = y2 + fromIntegral boy2 + fromIntegral h2/2
checkBoundaryBoxBottomIntersection :: V2 Float -> BoundaryBox -> V2 Float -> BoundaryBox -> Bool
checkBoundaryBoxBottomIntersection (V2 x1 y1) (BoundaryBox (w1, h1) (box1, boy1)) (V2 x2 y2) (BoundaryBox (w2, h2) (box2, boy2)) =
    top1 > bottom2 && bottom1 < bottom2 && right1 > left2 && left1 < right2
    where
        left1 = x1 + fromIntegral box1 - fromIntegral w1/2
        right1 = x1 + fromIntegral box1 + fromIntegral w1/2
        top1  = y1 + fromIntegral boy1 + fromIntegral h1/2
        bottom1 = y1 + fromIntegral boy1 - fromIntegral h1/2
        left2 = x2 + fromIntegral box2 - fromIntegral w2/2
        right2 = x2 + fromIntegral box2 + fromIntegral w2/2
        bottom2 = y2 + fromIntegral boy2 - fromIntegral h2/2
checkBoundaryBoxLeftIntersection :: V2 Float -> BoundaryBox -> V2 Float -> BoundaryBox -> Bool
checkBoundaryBoxLeftIntersection (V2 x1 y1) (BoundaryBox (w1, h1) (box1, boy1)) (V2 x2 y2) (BoundaryBox (w2, h2) (box2, boy2)) =
    right1 > left2 && left1 < left2 && bottom1 < top2 && top1 > bottom2
    where
        left1 = x1 + fromIntegral box1 - fromIntegral w1/2
        right1 = x1 + fromIntegral box1 + fromIntegral w1/2
        top1  = y1 + fromIntegral boy1 + fromIntegral h1/2
        bottom1 = y1 + fromIntegral boy1 - fromIntegral h1/2
        left2 = x2 + fromIntegral box2 - fromIntegral w2/2
        top2  = y2 + fromIntegral boy2 + fromIntegral h2/2
        bottom2 = y2 + fromIntegral boy2 - fromIntegral h2/2
checkBoundaryBoxRightIntersection :: V2 Float -> BoundaryBox -> V2 Float -> BoundaryBox -> Bool
checkBoundaryBoxRightIntersection (V2 x1 y1) (BoundaryBox (w1, h1) (box1, boy1)) (V2 x2 y2) (BoundaryBox (w2, h2) (box2, boy2)) =
    left1 < right2 && right1 > right2 && bottom1 < top2 && top1 > bottom2
    where
        left1 = x1 + fromIntegral box1 - fromIntegral w1/2
        right1 = x1 + fromIntegral box1 + fromIntegral w1/2
        top1  = y1 + fromIntegral boy1 + fromIntegral h1/2
        bottom1 = y1 + fromIntegral boy1 - fromIntegral h1/2
        right2 = x2 + fromIntegral box2 + fromIntegral w2/2
        top2  = y2 + fromIntegral boy2 + fromIntegral h2/2
        bottom2 = y2 + fromIntegral boy2 - fromIntegral h2/2

-- Transition easing
easeInOut :: Float -> Float
easeInOut t = t*t*(3 - 2*t)

lerp :: Float -> Float -> Float -> Float
lerp a b t = a + t * (b - a)

startTransition :: Float -> Float -> TransitionEvent -> System' ()
startTransition angle speed event = do
    cmapM_ $ \(Transition {}, e) -> destroy e (Proxy @(Transition, Position, Renderable))
    void $ newEntity (Transition { trProgress = 0, trAngle = angle, trSpeed = speed, trCoverEventFired = False, trEvent = event }
                    , Texture RenTexture { textureRef = "transition", animationFrame = Nothing }
                    , Position (V2 1000000 1000000)
                    , Layer 4
                    , IsVisible True)

-- Update positions based on velocity and delta time
stepPosition :: Float -> System' ()
stepPosition dT = cmap $ uncurry (stepPositionFormula dT)

spawnParticle :: Position -> Position -> String -> Int -> System' Entity
spawnParticle startPos endPos sref frameOffset = do
    TextureMap tmap <- get global
    let t = tmap Map.! sref
        frameCount' = maybe 1 frameCount (animation t)
        frameSpeed' = maybe 0.1 frameSpeed (animation t)
        (Position start) = startPos
        (Position end) = endPos
        vel = (end - start) ^/ ((fromIntegral frameCount' - fromIntegral frameOffset) * frameSpeed')
    newEntity (Particle endPos, startPos, Velocity vel, Texture RenTexture { textureRef = sref, animationFrame = Just frameOffset }, Layer 3, IsVisible True)