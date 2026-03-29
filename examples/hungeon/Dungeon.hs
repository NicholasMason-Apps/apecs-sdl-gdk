{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dungeon (stepDungeon) where

import Apecs
import Linear
import Types
import qualified Data.Set as Set
import Utils
import Data.Foldable (foldl')
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad
import qualified SDL
import Enemy
import GDK.Types
import GDK.Texture

handleEnemyCollisions :: Float -> System' ()
handleEnemyCollisions dT = cmapM_ $ \(Player, Position posP, v, bbp) -> do
    enemyRes <- cfold (\acc (Enemy _, Position posE, bbE, e) ->
        let
            (Position tempPosP) = stepPositionFormula dT (Position posP) v
        in
        if checkBoundaryBoxIntersection tempPosP bbp posE bbE && isNothing acc then
            Just e
        else
            acc) Nothing
    case enemyRes of
        Just e -> do
            ce <- cfold (\_ (CombatEnemy ce) -> Just ce) Nothing
            case ce of
                Just _ -> return ()
                Nothing -> do
                    et <- get e :: System' Enemy
                    let t = case enemyType et of
                            Reaper -> Texture RenTexture { textureRef = "reaper-idle", animationFrame = Just 0 }
                            Vampire -> Texture RenTexture { textureRef = "vampire-idle", animationFrame = Just 0 }
                            Skeleton -> Texture RenTexture { textureRef = "skeleton-idle", animationFrame = Just 0 }
                            GoldenReaper -> Texture RenTexture { textureRef = "golden-reaper-idle", animationFrame = Just 0 }
                    _ <- newEntity (CombatEnemy e, Position combatEnemyPos, t, Layer 2, IsVisible True)
                    set global $ CombatTurn PlayerTurn
                    startTransition (pi / 4) 1.0 ToCombat
        Nothing -> return ()

updatePlayerMovement :: System' ()
updatePlayerMovement = do
    KeysPressed ks <- get global
    -- liftIO $ putStrLn $ "Camera angle: " ++ show ca
    mTr <- cfold (\_ (Transition {}) -> Just ()) Nothing
    if isNothing mTr then
        cmapM_ $ \(Player, Velocity _, r, e) -> case r of
            Texture rt -> do
                let (V2 vx vy) = foldl' (\(V2 ax ay) dir -> case dir of
                        GkLeft -> V2 (ax - playerSpeed) ay
                        GkRight-> V2 (ax + playerSpeed) ay
                        GkUp -> V2 ax (ay + playerSpeed)
                        GkDown -> V2 ax (ay - playerSpeed)
                        _ -> V2 ax ay) (V2 0 0) (Set.toList ks)
                    newSprite
                        | vx == 0 && vy == 0 && textureRef rt /= "player-idle" = Texture rt { textureRef = "player-idle", animationFrame = Just 0 }
                        | (vx /= 0 || vy /= 0) && textureRef rt /= "player-walk" = Texture rt { textureRef = "player-walk", animationFrame = Just 0 }
                        | otherwise = Texture rt
                set e (Velocity (V2 vx vy))
                set e newSprite
            _ -> return ()
    else
        cmapM_ $ \(Player, r, e) -> case r of
            Texture rt -> do
                set e (Velocity (V2 0 0))
                set e (Texture rt { textureRef = "player-idle", animationFrame = Just 0 })
            _ -> return ()

ladderCollision :: System' ()
ladderCollision = cmapM_ $ \(Player, Position posP, bbP) -> do
    cmapM_ $ \(Ladder, Position posL, bbL) -> when (checkBoundaryBoxIntersection posP bbP posL bbL) $ do
        mTr <- cfold (\_ (Transition {}) -> Just ()) Nothing
        case mTr of
            Nothing -> startTransition (pi / 4) 1.0 ToNextLevel
            Just _ -> return ()

heartCollision :: System' ()
heartCollision = cmapM_ $ \(Player, Position posP, bbP, Health hp, ep) -> do
    cmapM_ $ \(Heart, Position posH, bbH, eh) -> when (checkBoundaryBoxIntersection posP bbP posH bbH) $ do
        set ep $ Health $ min 100 (hp + 50)
        destroy eh (Proxy @(Heart, Position, BoundaryBox, Item, Renderable, Layer, IsVisible))

stepDungeon :: Float -> System' ()
stepDungeon dT = do
    updatePlayerMovement
    stepEnemyAI
    blockPlayer dT
    stepPosition dT
    handleEnemyCollisions dT
    ladderCollision
    heartCollision

-- Block the player from moving into walls
blockPlayer :: Float -> System' ()
blockPlayer t = cmapM $ \(Player, Position posP, Velocity (V2 vx vy), bbp) -> do
    -- Get the next position for the player
    let (Position tempPos) = stepPositionFormula t (Position posP) (Velocity (V2 vx vy))
    -- For each wall, check for collision and adjust velocity accordingly
    cfoldM (\acc (Wall, Position posW, bbw) -> do
        let
            top = checkBoundaryBoxTopIntersection tempPos bbp posW bbw
            bottom = checkBoundaryBoxBottomIntersection tempPos bbp posW bbw
            left = checkBoundaryBoxLeftIntersection tempPos bbp posW bbw
            right = checkBoundaryBoxRightIntersection tempPos bbp posW bbw
            (Velocity (V2 avx avy)) = acc
        if (top && vy < 0) || (bottom && vy > 0) then
            return $ Velocity (V2 avx 0)
        else if (left && vx > 0) || (right && vx < 0) then
            return $ Velocity (V2 0 avy)
        else
            return acc) (Velocity (V2 vx vy))