{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications           #-}

module Settings (stepSettings) where

import Apecs
import Types
import qualified Data.Set as Set
import Linear
import qualified Data.Map as Map
import qualified Data.Vector as V
import Menu (buttonActions, posCheck)
import Control.Monad (when)
import Data.List (isInfixOf)
import Utils (startTransition)
import qualified SDL
import Data.Maybe (isJust)
import Cadence.Texture
import Cadence.Types

stepSettings :: Float -> System' ()
stepSettings dT = do
    stepButtonGroups
    stepButtons
    KeysPressed ks <- get global
    when (GkEsc `Set.member` ks) $ do
        startTransition (pi / 4) 1.0 ToMenu

stepButtonGroups :: System' ()
stepButtonGroups = do
    MousePosition (V2 mx my) <- get global
    KeysPressed ks <- get global
    TextureMap tmap <- get global
    cmapM_ $ \(SettingsUIElement, ButtonGroup group active, e) -> do
        V.mapM_ (\e' -> do
            Position (V2 x y) <- get e'
            r <- get e'
            case r of
                Texture rt -> do
                    let TextureData t _ = tmap Map.! textureRef rt
                        baseRef = (if "-hover" `isInfixOf` textureRef rt then take (length (textureRef rt) - 6) (textureRef rt) else textureRef rt)
                    info <- liftIO $ SDL.queryTexture t
                    let w = fromIntegral $ SDL.textureWidth info
                        h = fromIntegral $ SDL.textureHeight info
                    when (posCheck mx my x y w h && GkLMB `Set.member` ks) $ set e $ ButtonGroup group e'
                _ -> return ()
            ) group
        r <- get active
        case r of
            Texture rt -> do
                let baseRefA = (if "-hover" `isInfixOf` textureRef rt then take (length (textureRef rt) - 6) (textureRef rt) else textureRef rt)
                set active $ Texture rt { textureRef = baseRefA ++ "-hover" }
            _ -> return ()

stepButtons :: System' ()
stepButtons = do
    MousePosition (V2 mx my) <- get global
    KeysPressed ks <- get global
    TextureMap tmap <- get global
    cmapM_ $ \(SettingsUIElement, Button action, Position (V2 x y), r, e) -> case r of
        Texture rt -> do
            let
                TextureData t _ = tmap Map.! textureRef rt
                baseRef = (if "-hover" `isInfixOf` textureRef rt then take (length (textureRef rt) - 6) (textureRef rt) else textureRef rt)
            info <- liftIO $ SDL.queryTexture t
            let w = fromIntegral $ SDL.textureWidth info
                h = fromIntegral $ SDL.textureHeight info
            if posCheck mx my x y w h then
                set e $ Texture rt { textureRef = baseRef ++ "-hover" }
            else do
                isActive <- cfold (\_ (SettingsUIElement, ButtonGroup _ active) -> if active == e then Just () else Nothing) Nothing
                if isJust isActive then
                    set e $ Texture rt { textureRef = baseRef ++ "-hover" }
                else
                    set e $ Texture rt { textureRef = baseRef }
            when (posCheck mx my x y w h && GkLMB `Set.member` ks) $ buttonActions action
        _ -> return ()