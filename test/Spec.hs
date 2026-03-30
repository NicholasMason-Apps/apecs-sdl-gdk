{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Test.Hspec
import Test.QuickCheck
import GDK.Systems (initialise, makeWorld', stepAnimations)
import Apecs
import GDK.Types (Config(..), Renderable(..), Position(..), Time(..), Renderer(..), Window(..), TargetFPS(..), Camera(..), IsVisible(..), defaultConfig)
import GDK.Texture (TextureData(..), Animation(..), TextureMap(..), RenTexture(..), loadTexture)
import qualified SDL
import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Monad.Catch (try)
import GDK.Font (FontMap(..), loadFont)

makeWorld' []

testConfig :: Config
testConfig = defaultConfig { windowTitle = "Test Window" }

main :: IO ()
main = hspec $ do
    describe "GDK.Systems.initialise" $ do
        it "initialises SDL and creates a window and renderer" $ do
            w <- initWorld
            (window, renderer) <- initialise w testConfig
            size <- SDL.get (SDL.windowSize window)
            size `shouldBe` SDL.V2 800 600
            title <- SDL.get (SDL.windowTitle window)
            title `shouldBe` T.pack "Test Window"
            SDL.destroyRenderer renderer
            SDL.destroyWindow window
    describe "GDK.Systems.stepAnimations" $ do
        it "Correctly steps a single animation frame" $ do
            w <- initWorld
            (_, renderer) <- initialise w testConfig
            tex <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStatic (SDL.V2 1 1)
            let test1 = TextureData { texture = tex, animation = Just Animation { frameCount = 2, frameSpeed = 0.1, next = "test2" }}
                test2 = TextureData { texture = tex, animation = Just Animation { frameCount = 2, frameSpeed = 0.1, next = "test1" }}
            runSystem (do
                modify global $ \(TextureMap ts) -> TextureMap (Map.insert "test2" test2 (Map.insert "test1" test1 ts))
                _ <- newEntity (Texture (RenTexture { textureRef = "test1", animationFrame = Just 0 }), IsVisible True)
                stepAnimations 0.1
                en <- cfold (\acc r -> case r of
                        Texture t -> Just t
                        _ -> acc) Nothing
                liftIO $ en `shouldBe` (Just $ RenTexture { textureRef = "test1", animationFrame = Just 1 })) w
        it "Correctly loops to the next animation" $ do
            w <- initWorld
            (_, renderer) <- initialise w testConfig
            tex <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStatic (SDL.V2 1 1)
            let test1 = TextureData { texture = tex, animation = Just Animation { frameCount = 2, frameSpeed = 0.1, next = "test2" }}
                test2 = TextureData { texture = tex, animation = Just Animation { frameCount = 2, frameSpeed = 0.1, next = "test1" }}
            runSystem (do
                modify global $ \(TextureMap ts) -> TextureMap (Map.insert "test2" test2 (Map.insert "test1" test1 ts))
                _ <- newEntity (Texture (RenTexture { textureRef = "test1", animationFrame = Just 0 }), IsVisible True)
                stepAnimations 0.1
                stepAnimations 0.1
                en <- cfold (\acc r -> case r of
                        Texture t -> Just t
                        _ -> acc) Nothing
                liftIO $ en `shouldBe` (Just $ RenTexture { textureRef = "test2", animationFrame = Just 0 })) w
    describe "GDK.Texture.loadTexture" $ do
        it "Loads a valid texture into the TextureMap" $ do
            w <- initWorld
            (_, renderer) <- initialise w testConfig
            runSystem (do
                loadTexture renderer "test/resources/test.png" "testTex" Nothing
                TextureMap tm <- get global
                liftIO $ Map.member "testTex" tm `shouldBe` True) w
        it "Associates animation data with the loaded texture" $ do
            w <- initWorld
            (_, renderer) <- initialise w testConfig
            let anim = Animation { frameCount = 4, frameSpeed = 0.2, next = "nextTex" }
            runSystem (do
                loadTexture renderer "test/resources/test.png" "testTex" (Just anim)
                TextureMap tm <- get global
                case Map.lookup "testTex" tm of
                    Just td -> liftIO $ animation td `shouldBe` Just anim
                    Nothing -> liftIO $ expectationFailure "Texture not found in TextureMap") w
        it "Throws SDLException when loading a non-existent texture" $ do
            w <- initWorld
            (_, renderer) <- initialise w testConfig
            runSystem (do
                result <- try (loadTexture renderer "test/resources/nonexistent.png" "badTex" Nothing)
                case result of
                    Left (e :: SDL.SDLException) -> liftIO $ return () -- Expected exception, test passes
                    Right _ -> liftIO $ expectationFailure "Expected SDLException was not thrown") w
    describe "GDK.Font.loadFont" $ do
        it "Loads a valid font into the FontMap" $ do
            w <- initWorld
            _ <- initialise w testConfig
            runSystem (do
                loadFont "test/resources/Roboto-Black.ttf" "testFont" 24
                FontMap fm <- get global
                liftIO $ Map.member "testFont" fm `shouldBe` True) w
        it "Throws SDLException when loading a non-existent font" $ do
            w <- initWorld
            _ <- initialise w testConfig
            runSystem (do
                result <- try (loadFont "test/resources/nonexistent.ttf" "badFont" 24)
                case result of
                    Left (e :: SDL.SDLException) -> liftIO $ return () -- Expected exception, test passes
                    Right _ -> liftIO $ expectationFailure "Expected SDLException was not thrown") w