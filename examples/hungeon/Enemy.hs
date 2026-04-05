{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Enemy where

import Apecs
import System.Random
import Linear
import Types
import Utils
import Data.Maybe ( isNothing )
import Cadence.Types
import Cadence.Texture

enemyAggroRange :: Float
enemyAggroRange = 175.0

makeEnemy :: Enemy -> Position -> System' Entity
makeEnemy enemy pos = do
    let
        (sref, bbox) = case enemyType enemy of
            Reaper -> (Texture RenTexture { textureRef = "reaper-idle", animationFrame = Just 0 }, BoundaryBox (16, 26) (-1, -12))
            Vampire -> (Texture RenTexture { textureRef = "vampire-idle", animationFrame = Just 0 }, BoundaryBox (16, 30) (-6, -11))
            Skeleton -> (Texture RenTexture { textureRef = "skeleton-idle", animationFrame = Just 0 }, BoundaryBox (24, 26) (-2, -11))
            GoldenReaper -> (Texture RenTexture { textureRef = "golden-reaper-idle", animationFrame = Just 0 }, BoundaryBox (16, 26) (-1, -12))
    n <- case enemyType enemy of
        GoldenReaper -> liftIO $ randomRIO (150, 200)
        _ -> liftIO $ randomRIO (35, 50)
    newEntity (enemy, pos, Velocity (V2 0 0), sref, bbox, Health n, Layer 2, IsVisible True)

stepEnemyAI :: System' ()
stepEnemyAI = cmapM_ $ \(Player, Position posP) -> do
    cmapM $ \(Enemy _, Position posE, r) -> case r of
        Texture rt -> do
            let isInRange = distance posP posE <= enemyAggroRange
            ce <- cfold (\_ (CombatEnemy ce) -> Just ce) Nothing
            if isInRange && isNothing ce then do
                let dir = normalize (posP - posE)
                    newVel = dir ^* enemySpeed
                    sref' = case textureRef rt of
                        "reaper-idle" -> rt { textureRef = "reaper-walk", animationFrame = Just 0 }
                        "vampire-idle" -> rt { textureRef = "vampire-walk", animationFrame = Just 0 }
                        "skeleton-idle" -> rt { textureRef = "skeleton-walk", animationFrame = Just 0 }
                        "golden-reaper-idle" -> rt { textureRef = "golden-reaper-walk", animationFrame = Just 0 }
                        _ -> rt
                return (Velocity newVel, Texture sref')
            else
                let sref' = case textureRef rt of
                        "reaper-walk" -> rt { textureRef = "reaper-idle", animationFrame = Just 0 }
                        "vampire-walk" -> rt { textureRef = "vampire-idle", animationFrame = Just 0 }
                        "skeleton-walk" -> rt { textureRef = "skeleton-idle", animationFrame = Just 0 }
                        "golden-reaper-walk" -> rt { textureRef = "golden-reaper-idle", animationFrame = Just 0 }
                        _ -> rt
                in
                    return (Velocity (V2 0 0), Texture sref')
        r' -> return (Velocity (V2 0 0), r')