{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module GDK.Draw (draw, drawTexture, drawText) where

import qualified SDL
import qualified SDL.Font as TTF
import Apecs
import GDK.Types
import GDK.Texture
import GDK.Font (RenText(..), FontMap(..))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

{-|
Draw all 'Renderable' entities onto their appropriate layer.
This function is the typical draw function to pass to 'run', however you are free to implement your own
-}
draw :: forall w.
      (Has w IO TextureMap
      , Get w IO Window
      , Get w IO Renderable
      , Get w IO Position
      , Get w IO FontMap)
      => SDL.Renderer
      -> FPS
      -> System w ()
draw renderer fps = do
    Window win <- get global
    let window = fromMaybe (error "Window not initialised") win
    size <- SDL.get $ SDL.windowSize window
    maxLayer <- cfold (\acc r -> case r of
        Texture t -> max acc (textureLayer t)
        Text t -> max acc (fontLayer t)
        Points ps -> max acc $ V.foldl' (\acc' p -> max acc' (pointLayer p)) 0 ps
        ConnectedLines ls -> max acc $ V.foldl' (\acc' l -> max acc' (pointLayer l)) 0 ls
        SeparatedLines ls -> max acc $ V.foldl' (\acc' l -> max acc' (lineLayer l)) 0 ls
        Rectangles rs -> max acc $ V.foldl' (\acc' r' -> max acc' (rectLayer r')) 0 rs
        FilledRectangles rs -> max acc $ V.foldl' (\acc' r' -> max acc' (rectLayer r')) 0 rs) 0
    layers <- V.replicateM maxLayer (SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget size)
    TextureMap tm <- get global
    FontMap fm <- get global
    cmapM_ $ \(r, pos) -> case r of
        Texture t -> case Map.lookup (textureRef t) tm of
            Just td -> do
                let layerTex = layers V.! textureLayer t
                SDL.rendererRenderTarget renderer SDL.$= Just layerTex
                drawTexture renderer td pos (animationFrame t)
            Nothing -> return () -- Texture not found, skip drawing
        Text t -> case Map.lookup (fontRef t) fm of
            Just font -> do
                let layerTex = layers V.! fontLayer t
                SDL.rendererRenderTarget renderer SDL.$= Just layerTex
                drawText renderer t font pos
            Nothing -> return () -- Font not found, skip drawing
        Points ps -> V.mapM_ (\p -> do
            let layerTex = layers V.! pointLayer p
            SDL.rendererRenderTarget renderer SDL.$= Just layerTex
            drawPoint renderer p pos) ps
        ConnectedLines ls -> V.mapM_ (\l -> do
            let layerTex = layers V.! lineLayer l
            SDL.rendererRenderTarget renderer SDL.$= Just layerTex
            drawConnectedLine renderer l pos) ls
    SDL.rendererRenderTarget renderer SDL.$= Nothing
    V.mapM_ (\t -> do
        SDL.copy renderer t Nothing Nothing
        SDL.destroyTexture t) layers


drawPoint :: SDL.Renderer -> RenPoint -> Position -> System w ()
drawPoint r p (Position pos) = do
    SDL.rendererDrawColor r SDL.$= pointColour p
    SDL.drawPoint r (SDL.P $ round <$> pos)

{-|
Draw a 'Texture' given its 'TextureData' and 'Position'
If either the 'TextureData' does not have an 'Animation' or no frame index is provided, the texture will be drawn as if it was static.
-}
drawTexture :: SDL.Renderer -> TextureData -> Position -> Maybe Int -> System w ()
drawTexture r (TextureData t (Just a)) (Position pos) (Just n) = do
    info <- liftIO $ SDL.queryTexture t
    let w = fromIntegral $ SDL.textureWidth info
        h = fromIntegral $ SDL.textureHeight info
        fw = w `div` fromIntegral (frameCount a)
        srcRect = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral n * fw) 0)) (SDL.V2 fw h)
        dstRect = SDL.Rectangle (SDL.P (round <$> pos)) (SDL.V2 fw h)
    liftIO $ SDL.copy r t (Just srcRect) (Just dstRect)
drawTexture r (TextureData t _) (Position pos) _ = do
    info <- liftIO $ SDL.queryTexture t
    let w = fromIntegral $ SDL.textureWidth info
        h = fromIntegral $ SDL.textureHeight info
        pos' = SDL.Rectangle (SDL.P (round <$> pos)) (SDL.V2 w h)
    liftIO $ SDL.copy r t Nothing (Just pos')

-- | Draw text given its 'RenText', 'Font' and 'Position'
drawText :: SDL.Renderer -> RenText -> TTF.Font -> Position -> System w ()
drawText r t font (Position pos) = do
    (tex, size) <- generateSolidText r font (fontColour t) (fontText t)
    SDL.copy r tex Nothing (Just $ SDL.Rectangle (SDL.P (round <$> pos)) (fromIntegral <$> size))
    SDL.destroyTexture tex

generateSolidText :: MonadIO m => SDL.Renderer -> TTF.Font -> TTF.Color -> String -> m (SDL.Texture, SDL.V2 Int)
generateSolidText r font = generateText r font (TTF.solid font)

generateText :: MonadIO m => SDL.Renderer -> TTF.Font -> (TTF.Color -> T.Text -> m SDL.Surface) -> TTF.Color -> String -> m (SDL.Texture, SDL.V2 Int)
generateText r font f col str = do
    let t = T.pack str
    surface <- f col t
    tex <- liftIO $ SDL.createTextureFromSurface r surface
    SDL.freeSurface surface
    (w,h) <- liftIO $ TTF.size font t
    return (tex, SDL.V2 w h)

{-|
A global component which stores in a Vector a 'Texture' for each render layer.
The index of the vector corresponds to the render layer, so the texture at index 0 is the texture for render layer 0, and so on.
Lower layers are drawn first, and therefore can appear behind higher layers.
The texture for each layer is cleared at the start of each frame, and 'Renderables' are drawn onto the appropriate layer texture during the draw phase of the game loop.
-}
-- newtype RenderLayers = RenderLayers (V.Vector SDL.Texture)
-- instance Semigroup RenderLayers where
--     (RenderLayers q1) <> (RenderLayers q2) = RenderLayers (q1 V.++ q2)
-- instance Monoid RenderLayers where
--     mempty = RenderLayers V.empty
-- instance Component RenderLayers where type Storage RenderLayers = Global RenderLayers