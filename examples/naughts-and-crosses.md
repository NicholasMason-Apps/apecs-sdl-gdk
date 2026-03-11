This document breaks down a very small game written using `apecs-sdl-gdk`. We will be making naughts-and-crosses (or tic-tac-toe depending on where you are from). The main focus of this document will be to highlight how to use `apecs-sdl-gdk`, a (brief) introduction into how to use Apecs, and explain what some of the `apecs-sdl-gdk` functions do under-the-hood. I **strongly** recommend you write code yourself as you follow along, as it will help a lot to solidfy the basic concepts.

With all that said, let's start writing some code! To start with, we will need some language extensions since Apecs makes use of a lot of extensions for its inner workings.

```haskell
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
```

Next, we need some imports. To work with Apecs and create some game logic, we simply import Apecs directly

```haskell
import Apecs
```

Following this, we import `apecs-sdl-gdk`. `GDK` re-exports everything for you to use.

```haskell
import GDK
```

Next we want to import `linear` for working with multi-dimensional vectors.

```haskell
import Linear
```

Finally, we will import TODO:

```haskell
import qualified Data.Set as Set
import qualified SDL
```

Now with the imports complete, we can define some Components needed for our game logic! If you are not familiar with the notion of an ECS, I would recommend reading the [Apecs paper](https://github.com/jonascarpay/apecs/blob/master/apecs/prepub.pdf) (as well as the other resources linked in the `README.md`) to get an understanding of how it works. In short, an ECS works by having you create Entities which inhabit the game world, and have tied to them a composition of Components which are used to represent properties of that entity. You then write Systems to manipulate all entities with a specific subset of components to write game logic.

In Apecs, Components come in the form of Haskell types being instances of the Component class, and each have their own store specified. A store is simply the data structure used for it. The most common one you will use is `Map`, as it allows for each entity to potentially have an instance of this component. There are others, however, such as: `Global`, which allows data to be stored irrespective to any entity; `Unique`, which allows at most one entity to hold the component; and `Cache`, which wraps around another store to allow for O(1) reads and writes on it.

For the purpose of naughts-and-crosses, we will need to store: the current board state, whether it is the player's turn or AI's turn, and the naughts or crosses to draw on the screen.

For the first two, we will make use of the `Global` store. When defining a Component that uses `Global`, we need to also make it an instance of `Monoid`, which by extension depending on your GHC version also requires making it an instance of `Semigroup`. The reason for requiring a `Monoid` instance is to gain access to `mempty`, as `mempty` is used to derive the starting state of the `Global` store.

For the game's board state, we will also define a Sum type to help with the representation.

Finally, we will also define a `Global` Component for the mouse's position and a `Set` to store the mouse input events which may occur. Whilst redundant for this toy example of naughts-and-crosses, from personal experience I have found using a Global store for all the input events you may want to capture is extremely useful, as it allows for game logic to be separated away from the event handler function.

```haskell
data Turn = PlayerTurn | AITurn deriving Show
instance Semigroup Turn where
    _ <> t2 = t2
instance Monoid Turn where
    mempty = PlayerTurn
instance Component Turn where type Storage Turn = Global Turn

data BoardEntry = Naught | Cross | Empty deriving Show
instance Semigroup BoardEntry where
    Empty <> b = b
    b <> Empty = b
    b <> _ = b

newtype Board = Board [[BoardEntry]] deriving Show
instance Semigroup Board where
    b1 <> b2 = b1 <> b2
instance Monoid Board where
    mempty = [
        [Empty, Empty, Empty],
        [Empty, Empty, Empty],
        [Empty, Empty, Empty]
    ]

newtype MousePosition = MousePosition (V2 Float)
instance Semigroup MousePosition where
    (MousePosition m1) <> (MousePosition m2) = MousePosition (m1 + m2)
instance Monoid MousePosition where
    mempty = MousePosition (V2 0 0) 
instance Component MousePosition where type Storage MousePosition = Global MousePosition

newtype MouseButtons = MouseButtons (Set.Set SDL.MouseButton)
```

With those Global Components out of the way, we can focus on storing the actual naughts and crosses to draw on the screen. Normally, this would require us to create new Components to store their positions, drawing data, etc., however `apecs-sdl-gdk` exposes a nice way to capture this for minimal effort. `apecs-sdl-gdk` exposes a `Renderable` sum type and a `Position` component, which are both stored in a Map. The actual definitions of both are shown below, and I will go into more detail about what they do alongside those definitions.

```sourceCode haskell
data RenTexture = RenTexture
    { textureRef :: String
    -- ^ Identifier for the texture to render
    , textureLayer :: Int
    -- ^ layer for draw order
    , animationFrame :: Maybe Int
    -- ^ Frame index for animations, if applicable
    } deriving (Eq, Show)

data RenText = RenText
    { fontRef :: String
    , displayText :: String
    , textColour :: TTF.Color
    , textLayer :: Int
    } deriving (Show, Eq)

data RenPoint = RenPoint
    { pointColour :: SDL.V4 Word8
    , pointLayer :: Int
    } deriving (Show, Eq)

data RenLine = RenLine
    { lineColour :: SDL.V4 Word8
    , lineLayer :: Int
    , lineX :: Float -- ^ X coordinate of the line's ending point
    , lineY :: Float -- ^ Y coordinate of the line's ending point
    } deriving (Show, Eq)

data RenRectangle = RenRectangle
    { rectSize :: SDL.V2 Float -- ^ Width and height of the rectangle
    , rectColour :: SDL.V4 Word8
    , rectLayer :: Int
    } deriving (Show, Eq)

-- | Represents an entity that can be rendered
data Renderable = Texture RenTexture
                | Text RenText
                | Point RenPoint
                | Line RenLine
                | Rectangle RenRectangle
                | FilledRectangle RenRectangle
                deriving (Eq, Show)
instance Component Renderable where type Storage Renderable = Map Renderable
```

As you can see, the main type here is our Component `Renderable`. Renderable simply represents a single entity which is wanting to be drawn, and provides a Sum type accordingly to specify what it is you want to render. For `Point`, `Line`, `Rectangle`, and `FilledRectangle`, what they provide and use their respecitve `Ren...` types for is rather self-explanatory. However, for `Texture` and `Text`, is it a bit more nuanced. Both Texture and Text make use of a reference String, which is used as an identifier into the Global `TextureMap` and `FontMap` that `apecs-sdl-gdk` exposes respectively. The purpose of this is to conform to the flyweight design pattern, and reduce the memory load of games by removing any entities storing the same texture or font data. To interface with these and load textures/fonts into the game, `apecs-sdl-gdk` also exposes the two following functions:

```sourceCode haskell
-- | Load a font into the 'FontMap'
loadFont :: (..) => FilePath -> String -> Int -> SystemT w m ()
loadFont path ident size = ...

-- | Loads a texture into the 'TextureMap' with an associated identifier and optional animation data
loadTexture :: (..) => SDL.Renderer -> FilePath -> String -> Maybe Animation -> SystemT w m ()
loadTexture r path ident anim = ...
```

These functions handle the loading and storing of a Texture or Font for you, and if you pass an invalid file path, they will throw an error at runtime. 

Whilst the Renderable Component handles a lot of the heavy lifting for you, there is one other Component needed to complete the rendering pipeline. This Component is a Position Component, which simply represents the 2D position of the entity within the game space. In `apecs-sdl-gdk` it is defined as the following:

```sourceCode haskell
newtype Position = Position (V2 Float) deriving (Show, Eq)
instance Component Position where type Storage Position = Map Position
```

With these two Components, we form the two fundamental Components needed for the rendering pipeline of `apecs-sdl-gdk`, and is exposed as a single function `draw`. What `draw` does is it takes all entities which have a `Renderable` and `Position` component, and draws all the entities onto the game window, ensuring to apply the given colour and render it onto the correct layer. This means that all the rendering is done for you, and all you need to do is write game logic!

Alongside the two most important Components, `Renderable` and `Position`, `apecs-sdl-gdk` also exposes some other Components which are important to know. These are all listed and explained below

```sourceCode haskell
-- | Configuration settings for the game upon initialisation
data Config = Config
    {
        windowTitle :: String, -- ^ Title of the game window
        windowDimensions :: (Int, Int), -- ^ Width and height of the game window in pixels
        backgroundColor :: V4 Word8, -- ^ Background color of the game window as an RGBA value
        targetFPS :: TargetFPS, -- ^ Desired FPS for the game loop
        showFPS :: Maybe String -- ^ Whether to display the current FPS on the screen, and if so, the font to use
    }
instance Semigroup Config where
    _ <> c2 = c2
instance Monoid Config where
    mempty = defaultConfig
instance Component Config where type Storage Config = Global Config 
```

`Config` is used to configure the game window, and is passed during initialisation, and then stored globally such that we can access it in the future if needed. We also expose a `defaultConfig`, which is as follows:

```sourceCode haskell
defaultConfig :: Config
defaultConfig = Config
    { windowTitle = "GDK Game"
    , windowDimensions = (800, 600)
    , backgroundColor = V4 0 0 0 255
    , targetFPS = VSync
    , showFPS = Just "Roboto-Regular"
    }
```

Following `Config`, we also have these other `Global` Components

```sourceCode haskell
newtype Camera = Camera (V2 Int)
instance Semigroup Camera where
    (Camera c1) <> (Camera c2) = Camera (c1 + c2)
instance Monoid Camera where
    mempty = Camera $ V2 0 0
instance Component Camera where type Storage Camera = Global Camera

newtype Time = Time Float deriving (Show, Eq, Num)
instance Semigroup Time where
    (Time t1) <> (Time t2) = Time (t1 + t2)
instance Monoid Time where
    mempty = Time 0
instance Component Time where type Storage Time = Global Time

newtype Renderer = Renderer (Maybe SDL.Renderer)
instance Semigroup Renderer where
    (Renderer r1) <> (Renderer r2) = Renderer r1
instance Monoid Renderer where
    mempty = Renderer Nothing
instance Component Renderer where type Storage Renderer = Global Renderer

newtype Window = Window (Maybe SDL.Window)
instance Semigroup Window where
    (Window w1) <> (Window w2) = Window w1
instance Monoid Window where
    mempty = Window Nothing
instance Component Window where type Storage Window = Global Window
```

`Camera` is used to store the x and y offset to apply to the renderer when drawing. An example of when you may want to use this is to lock the game window to always be centre on your player. `Time` is used to store the total accumulated time of the game. `Renderer` is used to store the SDL rendering context, and similarly `Window` is used to store the SDL window context.

With that all said and done, we can now start implementing our naughts-and-crosses game logic. We will first start by defining our Game's configuration.

```haskell
config :: Config
config = defaultConfig { windowTitle = "Naughts and Crosses"
                       , windowDimensions = (600,600)
                       , showFPS = Nothing }
```

The only notable configuration here is the window dimensions. For the sake of this toy example, we will make the entire window the naughts and crosses board, which means our mouse position will always be in one of the 9 sections of the board.

