{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Combat where

import Apecs
import Linear
import Types
import Utils
import Control.Monad
import Data.Maybe ( isJust, fromMaybe, isNothing )
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified SDL
import GDK.Types
import GDK.Texture
import GDK.Font

playerKnifeAttackFrames :: Set.Set Int
playerKnifeAttackFrames = Set.fromList [7]

playerMagicAttackFrames :: Set.Set Int
playerMagicAttackFrames = Set.fromList [6]

enemySkeletonAttackFrames :: Set.Set Int
enemySkeletonAttackFrames = Set.fromList [7]

enemyVampireAttackFrames :: Set.Set Int
enemyVampireAttackFrames = Set.fromList [11]

enemyReaperAttackFrames :: Set.Set Int
enemyReaperAttackFrames = Set.fromList [6, 11]

enemyGoldenReaperAttackFrames :: Set.Set Int
enemyGoldenReaperAttackFrames = Set.fromList [6, 11]

playerShieldFrames :: Set.Set Int
playerShieldFrames = Set.fromList [1,2,3]

playerDamage :: Int
playerDamage = 20

enemyDamage :: Int
enemyDamage = 5

bossDamage :: Int
bossDamage = 15

stepPlayerTurn :: Float -> System' ()
stepPlayerTurn dT = do
    KeysPressed ks <- get global
    uiState <- get global :: System' UIState
    case uiState of
        CombatAttackSelectUI -> do
            when (GkSpace `Set.member` ks) $ do
                set global $ KeysPressed (GkSpace `Set.delete` ks)
                set global $ CombatTurn PlayerAttacking
                cmapM_ $ \(CombatPlayer, s) -> do
                    set s (Texture RenTexture { textureRef = "player-knife-attack", animationFrame = Just 0 })
                    set s (Position (combatEnemyPos - V2 tileSize 0))
                    parryUI
            when (GkE `Set.member` ks) $ do
                set global CombatMagicSelectUI
                cmap $ \(CombatUI, r) -> case r of
                    Texture rt -> Texture rt { textureRef = "combat-magic-select-ui" }
                    _ -> r
                set global $ KeysPressed (GkE `Set.delete` ks)
        CombatMagicSelectUI -> do
            when (GkE `Set.member` ks) $ do
                set global $ CombatTurn PlayerAttacking
                set global CombatAttackSelectUI
                set global $ KeysPressed (GkE `Set.delete` ks)
                cmapM_ $ \(CombatPlayer, s) -> set s (Texture RenTexture { textureRef = "player-fire-attack", animationFrame = Just 0 })
                parryUI
            when (GkQ `Set.member` ks) $ do
                set global $ CombatTurn PlayerAttacking
                set global CombatAttackSelectUI
                set global $ KeysPressed (GkQ `Set.delete` ks)
                cmapM_ $ \(CombatPlayer, s) -> set s (Texture RenTexture { textureRef = "player-prismatic-attack", animationFrame = Just 0 })
                parryUI
            when (GkEsc `Set.member` ks) $ do
                set global CombatAttackSelectUI
                attackUI
                set global $ KeysPressed (GkEsc `Set.delete` ks)

parryUI :: System' ()
parryUI = cmap $ \(CombatUI, r) -> case r of
    Texture rt -> Texture rt { textureRef = "combat-parry-ui" }
    _ -> r

attackUI :: System' ()
attackUI = cmap $ \(CombatUI, r) -> case r of
    Texture rt -> Texture rt { textureRef = "combat-attack-select-ui" }
    _ -> r

stepPlayerAttack :: Float -> System' ()
stepPlayerAttack dT = do
    cmapM_ $ \(CombatPlayer, r, e) -> case r of
        Texture rt -> do
            particle <- cfold (\_ (CombatAttackParticle e) -> Just e) Nothing
            when (isNothing particle && (textureRef rt == "player-fire-attack" || textureRef rt == "player-prismatic-attack") && (fromMaybe 0 (animationFrame rt) `Set.member` playerMagicAttackFrames)) $ do
                if textureRef rt == "player-fire-attack" then do
                    particle <- spawnParticle (Position (combatPlayerPos + V2 (tileSize/2) 16)) (Position combatEnemyPos) "particle-fire" 11
                    void $ newEntity (CombatAttackParticle particle)
                else when (textureRef rt == "player-prismatic-attack") $ do
                    particle <- spawnParticle (Position (combatPlayerPos + V2 (tileSize/2) 16)) (Position combatEnemyPos) "particle-prismatic" 13
                    void $ newEntity (CombatAttackParticle particle)
            Particle (Position destPos) <- case particle of
                Just p -> get p :: System' Particle
                Nothing -> return $ Particle (Position (V2 0 0))
            Position currPos <- case particle of
                Just p -> do
                    Position pos <- get p :: System' Position
                    return $ Position pos
                Nothing -> return $ Position (V2 100 100)
            let attackHitCondition = (fromMaybe 0 (animationFrame rt) `Set.member` playerKnifeAttackFrames && textureRef rt == "player-knife-attack")
                                    || (norm (destPos - currPos) < 20)
            when attackHitCondition $ cmapM_ $ \(CombatEnemy e', r', ce) -> case r' of
                Texture rt' -> do
                    enemy <- get e' :: System' Enemy
                    Health hp <- get e' :: System' Health
                    when (textureRef rt' == "skeleton-idle" || textureRef rt' == "vampire-idle" || textureRef rt' == "reaper-idle" || textureRef rt' == "golden-reaper-idle") $ do
                        if hp - playerDamage > 0 then
                            case enemyType enemy of
                                Reaper -> when (textureRef rt' /= "reaper-hit") $ do
                                    modify e' $ \(Health hp) -> Health (hp - playerDamage)
                                    set ce (Texture rt { textureRef = "reaper-hit", animationFrame = Just 1 })
                                Vampire -> when (textureRef rt' /= "vampire-hit") $ do
                                    modify e' $ \(Health hp) -> Health (hp - playerDamage)
                                    set ce (Texture rt { textureRef = "vampire-hit", animationFrame = Just 1 })
                                Skeleton -> when (textureRef rt' /= "skeleton-hit") $ do
                                    modify e' $ \(Health hp) -> Health (hp - playerDamage)
                                    set ce (Texture rt { textureRef = "skeleton-hit", animationFrame = Just 1 })
                                GoldenReaper -> when (textureRef rt' /= "golden-reaper-hit") $ do
                                    modify e' $ \(Health hp) -> Health (hp - playerDamage)
                                    set ce (Texture rt { textureRef = "golden-reaper-hit", animationFrame = Just 1 })
                        else
                            case enemyType enemy of
                                Reaper -> when (textureRef rt' /= "reaper-death") $ do
                                    set ce (Texture rt { textureRef = "reaper-death", animationFrame = Just 1 })
                                    set global $ CombatTurn PlayerWin
                                Vampire -> when (textureRef rt' /= "vampire-death") $ do
                                    set ce (Texture rt { textureRef = "vampire-death", animationFrame = Just 1 })
                                    set global $ CombatTurn PlayerWin
                                Skeleton -> when (textureRef rt' /= "skeleton-death") $ do
                                    set ce (Texture rt { textureRef = "skeleton-death", animationFrame = Just 1 })
                                    set global $ CombatTurn PlayerWin
                                GoldenReaper -> when (textureRef rt' /= "golden-reaper-death") $ do
                                    set ce (Texture rt { textureRef = "golden-reaper-death", animationFrame = Just 1 })
                                    set global $ CombatTurn PlayerWin
                _ -> return ()
            when (textureRef rt == "player-idle" && isNothing particle) $ do
                set global $ CombatTurn EnemyTurn
                set e (Position combatPlayerPos)
        _ -> return ()

stepEnemyAttack :: Float -> System' ()
stepEnemyAttack dT = do
    cmapM_ $ \(CombatEnemy e', r, e) -> case r of
        Texture rt -> do
            when (textureRef rt == "skeleton-idle" || textureRef rt == "vampire-idle" || textureRef rt == "reaper-idle" || textureRef rt == "golden-reaper-idle") $ do
                set global $ CombatTurn PlayerTurn
                attackUI
                set e (Position combatEnemyPos)
            enemy <- get e' :: System' Enemy
            KeysPressed ks <- get global
            cmapM_ $ \(CombatPlayer, r', cp) -> case r' of
                Texture rt' -> do
                    when (textureRef rt' == "player-idle" && (GkF `Set.member` ks)) $ do
                        set cp (Texture rt' { textureRef = "player-shield", animationFrame = Just 0 })
                        set global $ ShieldCooldown 1.0
                _ -> return ()
            case enemyType enemy of
                Skeleton -> when (fromMaybe 0 (animationFrame rt) `Set.member` enemySkeletonAttackFrames) hitPlayer
                Vampire -> when (fromMaybe 0 (animationFrame rt) `Set.member` enemyVampireAttackFrames) hitPlayer
                Reaper -> when (fromMaybe 0 (animationFrame rt) `Set.member` enemyReaperAttackFrames) hitPlayer
                GoldenReaper -> when (fromMaybe 0 (animationFrame rt) `Set.member` enemyGoldenReaperAttackFrames) hitPlayer
            where
            hitPlayer = cmapM_ $ \(CombatPlayer, r', cp) -> case r' of
                Texture rt' -> do
                    when (textureRef rt' /= "player-hit") $ do
                        if textureRef rt' == "player-shield" && fromMaybe 0 (animationFrame rt') `Set.member` playerShieldFrames then do
                            modify global $ \(ShieldCooldown _) -> ShieldCooldown 0
                            void $ newEntity (FloatingText 0 1.0, Position (combatPlayerPos + V2 0 20), Text RenText { fontRef = "Roboto-Regular", displayText = "Blocked!" }, Colour (V4 255 255 255 255), Layer 3, IsVisible True, Velocity (V2 0 20))
                        else cmapM_ $ \(Player, Health hp) -> if hp - enemyDamage > 0 then do
                                cmap $ \(Player, Health hp) -> Health (hp - enemyDamage)
                                set cp (Texture rt' { textureRef = "player-hit", animationFrame = Just 1 })
                            else do
                                set cp (Texture rt' { textureRef = "player-hit", animationFrame = Just 1 })
                                set global $ CombatTurn EnemyWin
                _ -> return ()
        _ -> return ()
stepEnemyTurn :: Float -> System' ()
stepEnemyTurn dT = do
    cmapM_ $ \(CombatEnemy _, r, e) -> case r of
        Texture rt -> do
            case textureRef rt of
                "skeleton-idle" -> do
                    set e (Texture rt { textureRef = "skeleton-attack", animationFrame = Just 0 })
                    set e (Position (combatPlayerPos + V2 tileSize 0))
                    set global $ CombatTurn EnemyAttacking
                "vampire-idle"  -> do
                    set e (Texture rt { textureRef = "vampire-attack", animationFrame = Just 0 })
                    set e (Position (combatPlayerPos + V2 tileSize 0))
                    set global $ CombatTurn EnemyAttacking
                "reaper-idle"   -> do
                    set e (Texture rt { textureRef = "reaper-attack", animationFrame = Just 0 })
                    set e (Position (combatPlayerPos + V2 tileSize 0))
                    set global $ CombatTurn EnemyAttacking
                "golden-reaper-idle"   -> do
                    set e (Texture rt { textureRef = "golden-reaper-attack", animationFrame = Just 0 })
                    set e (Position (combatPlayerPos + V2 tileSize 0))
                    set global $ CombatTurn EnemyAttacking
                _               -> return ()
        _ -> return ()

stepPlayerWin :: Float -> System' ()
stepPlayerWin dT = cmapM_ $ \(CombatEnemy _, r) -> case r of
    Texture rt -> do
        cmapIf (\(CombatPlayer, r') -> case r' of
                Texture rt' -> textureRef rt' == "player-idle"
                _ -> False
            ) (\CombatPlayer -> Position combatPlayerPos)
        when (textureRef rt == "vampire-death" || textureRef rt == "skeleton-death" || textureRef rt == "reaper-death" || textureRef rt == "golden-reaper-death") $ do
            TextureMap tmap <- get global
            let TextureData t ma = tmap Map.! textureRef rt
                anim = fromMaybe (error "No animation data for this texture") ma
            existsTransition <- cfold (\_ (Transition {}) -> Just ()) Nothing
            when (fromMaybe 0 (animationFrame rt) + 1 >= frameCount anim && isNothing existsTransition) $ startTransition (pi / 4) 1.0 ToDungeon
    _ -> return ()

stepEnemyWin :: Float -> System' ()
stepEnemyWin dT = do
    existsTransition <- cfold (\_ (Transition {}) -> Just ()) Nothing
    when (isNothing existsTransition) $ startTransition (pi / 4) 1.0 ToMenu

stepCombat :: Float -> System' ()
stepCombat dT = do
    ce <- cfold (\_ (CombatEnemy ce) -> Just ce) Nothing
    CombatTurn turn <- get global
    playerHealth <- cfold (\_ (Player, Health hp) -> Just hp) Nothing
    when (isJust playerHealth && fromMaybe 0 playerHealth <= 0) $ liftIO $ putStrLn "Player has been defeated!"
    stepPosition dT
    case ce of
        Nothing -> return ()
        Just e -> case turn of
            PlayerTurn -> stepPlayerTurn dT
            EnemyTurn -> stepEnemyTurn dT
            PlayerAttacking -> stepPlayerAttack dT
            EnemyAttacking -> stepEnemyAttack dT
            PlayerWin -> stepPlayerWin dT
            EnemyWin -> stepEnemyWin dT