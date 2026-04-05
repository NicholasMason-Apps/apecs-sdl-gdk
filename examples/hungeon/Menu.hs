{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications           #-}

module Menu (stepMenu, buttonActions, posCheck) where

import Apecs
import Types
import Utils (startTransition)
import Control.Monad (when)
import qualified Data.Set as Set
import Linear
import qualified Data.Map as Map
import Data.List (isInfixOf)
import Data.Aeson
import qualified SDL
import qualified Data.ByteString.Lazy as BL
import Cadence.Types
import Cadence.Texture

stepMenu :: Float -> System' ()
stepMenu dT = do
    stepButtons
    -- KeysPressed ks <- get global
    -- when (GkSpace `Set.member` ks) $ do
    --     startTransition (pi / 4) 1.0 StartDungeon

stepButtons :: System' ()
stepButtons = do
    MousePosition (V2 mx my) <- get global
    KeysPressed ks <- get global
    TextureMap tmap <- get global
    cmapM_ $ \(MainMenuUIElement, Button action, Position (V2 x y), r :: Renderable, e) -> case r of
        Texture rt -> do
            let
                TextureData t ma = tmap Map.! textureRef rt
                baseRef = (if "-hover" `isInfixOf` textureRef rt then take (length (textureRef rt) - 6) (textureRef rt) else textureRef rt)
            info <- liftIO $ SDL.queryTexture t
            let w = fromIntegral $ SDL.textureWidth info
                h = fromIntegral $ SDL.textureHeight info
            if posCheck mx my x y w h then
                set e $ Texture rt { textureRef = baseRef ++ "-hover" }
            else
                set e $ Texture rt { textureRef = baseRef }
            when (posCheck mx my x y w h && GkLMB `Set.member` ks) $ buttonActions action
        _ -> return ()

buttonActions :: ButtonAction -> System' ()
buttonActions StartGameButton = startTransition (pi / 4) 1.0 StartDungeon
buttonActions SettingsButton = startTransition (pi / 4) 1.0 ToSettings
buttonActions BackToTitleButton = startTransition (pi / 4) 1.0 ToMenu
buttonActions FullscreenButton = do
    settings <- get global :: System' Settings
    set global (settings { fullscreen = True })
    liftIO $ BL.writeFile "settings.json" (encode settings { fullscreen = True })
buttonActions WindowedButton = do
    settings <- get global :: System' Settings
    set global (settings { fullscreen = False })
    liftIO $ BL.writeFile "settings.json" (encode settings { fullscreen = False })
-- buttonActions _ = return ()

posCheck :: (Fractional a1, Fractional a2, Integral a3, Integral a4, Ord a1,  Ord a2) => a1 -> a2 -> a1 -> a2 -> a3 -> a4 -> Bool
posCheck mx my x y w h = mx >= x && mx <= x + fromIntegral w &&
                         my >= y - fromIntegral h && my <= y