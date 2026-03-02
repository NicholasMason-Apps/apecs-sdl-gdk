{-# LANGUAGE TypeFamilies #-}

module GDK.Draw (draw, drawTexture, drawText) where

import qualified SDL
import qualified SDL.Font as TTF
import Apecs
import GDK.Types
import GDK.Texture
import GDK.Font (RenText(..))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Vector as V
import qualified Data.Text as T

{-|
Draw all 'Renderable' entities onto their appropriate layer.
This function is the typical draw function to pass to 'run', however you are free to implement your own
-}
draw :: SDL.Renderer -> FPS -> System w ()
draw renderer fps = return ()
    let layers = V.empty :: V.Vector SDL.Texture
    

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
    (tex, size) <- generateSolidText r font (colour t) (text t)
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