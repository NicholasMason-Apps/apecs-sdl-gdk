# Noughts & Crosses Tutorial

This document breaks down a very small game written using `apecs-sdl-gdk`. We will be making noughts-and-crosses (or tic-tac-toe depending on where you are from). The main focus of this document will be to highlight how to use `apecs-sdl-gdk`, a (brief) introduction into how to use Apecs, and explain what some of the `apecs-sdl-gdk` functions do under-the-hood. Additionally, I will also highlight some tips for game development using Apecs from my experience! 

I **strongly** recommend you write code yourself as you follow along, as it will help a lot to solidfy the basic concepts. Additionally, Apecs itself has a [tutorial](https://github.com/jonascarpay/apecs/blob/master/examples/Shmup.md), which whilst it does not use `apecs-sdl-gdk`, I would also highly recommend walking through it as well for a more comprohensive overview of how to use Apecs specifically. 

## Preliminary Setup

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

Next, we need some imports. To work with Apecs and create some game logic, we simply import Apecs directly:

```haskell
import Apecs
```

Following this, we import `apecs-sdl-gdk`. The only thing to note here is that, since we will also reuse the `initialise` function name later on, we also import `GDK.Systems` qualified.

```haskell
import GDK.Systems hiding (initialise)
import qualified GDK.Systems as GDK
import GDK.Types
import GDK.Draw
```

Next we want to import `linear` for working with multi-dimensional vectors.

```haskell
import Linear
```

Finally, we will import some other utilities to help us later:

```haskell
import qualified Data.Set as Set
import qualified SDL
import Control.Monad (when, void)
import System.Random
import Data.Maybe (isJust)
import Data.Foldable (foldl')
```

## Game Components

Now with the imports complete, we can define some Components needed for our game logic! If you are not familiar with the notion of an ECS, I would recommend reading the [Apecs paper](https://github.com/jonascarpay/apecs/blob/master/apecs/prepub.pdf) (as well as the other resources linked in the `README.md`) to get an understanding of how it works. In short, an ECS works by having you create Entities which inhabit the game world, and have tied to them a composition of Components which are used to represent properties of that entity. You then write Systems to manipulate all entities with a specific subset of components to write game logic. For example, suppose we have three entities. One entity holds `Player` and `Position` components, and the other two hold `Enemy` and `Position` components. This means in our world we have three entities and three types of components. Then, when writing systems, we could update **all** Entities which have a `Position` component to move them, and then later on update only the Entity with the `Player` component to decrement some internal health.

In Apecs, Components come in the form of Haskell types being instances of the Component class, and each have their own store specified. A store is simply the data structure used for that component. The most common one you will use is `Map`, as it allows for each entity to potentially have an instance of this component. There are others, however, such as: `Global`, which allows data to be stored irrespective to any entity (more on that later); `Unique`, which allows at most one entity to hold the component; and `Cache`, which wraps around another store to allow for O(1) reads and writes on it.

For the purpose of naughts-and-crosses, we will need to store: the current board state, the state of the game, the naughts or crosses to draw on the screen, player inputs, and for some visuals which will be explained later, a countdown timer.

For the first two, we will make use of the `Global` store. When defining a Component that uses `Global`, we need to also make it an instance of `Monoid`, which by extension depending on your GHC version also requires making it an instance of `Semigroup`. The reason for requiring a `Monoid` instance is to gain access to `mempty`, as `mempty` is used to derive the starting state of the `Global` store.

For the game's board state, we will also define a Sum type to help with the representation.

We will also define a `Global` Component for the mouse's position and a `Set` to store the mouse input events which may occur. Whilst redundant for this toy example of naughts-and-crosses, from personal experience I have found using a Global store for all the input events you may want to capture is extremely useful, as it allows for game logic to be separated away from the event handler function.

Finally, we will use `Unique` for storing our countdown timer. For our game, we will only ever need at most one timer at any given moment, making the `Unique` store a perfect candidate for its storage.

```haskell
data Turn = PlayerTurn | AITurn | Win | Reset | CheckWinPlayer | CheckWinAI deriving Show
instance Semigroup Turn where
    _ <> t2 = t2
instance Monoid Turn where
    mempty = PlayerTurn
instance Component Turn where type Storage Turn = Global Turn

data BoardEntry = Naught | Cross | Empty deriving (Show, Eq)
instance Semigroup BoardEntry where
    Empty <> b = b
    b <> Empty = b
    b <> _ = b

newtype Board = Board [[BoardEntry]] deriving Show
instance Semigroup Board where
    b1 <> b2 = b1 <> b2
instance Monoid Board where
    mempty = Board $ [
            [Empty, Empty, Empty],
            [Empty, Empty, Empty],
            [Empty, Empty, Empty]
        ]
instance Component Board where type Storage Board = Global Board

newtype MousePosition = MousePosition (V2 Float)
instance Semigroup MousePosition where
    (MousePosition m1) <> (MousePosition m2) = MousePosition (m1 + m2)
instance Monoid MousePosition where
    mempty = MousePosition (V2 0 0) 
instance Component MousePosition where type Storage MousePosition = Global MousePosition

newtype MouseButtons = MouseButtons (Set.Set SDL.MouseButton)
instance Semigroup MouseButtons where
    (MouseButtons mbs1) <> (MouseButtons mbs2) = MouseButtons (mbs1 `Set.union` mbs2)
instance Monoid MouseButtons where
    mempty = MouseButtons Set.empty
instance Component MouseButtons where type Storage MouseButtons = Global MouseButtons

newtype Countdown = Countdown Float deriving (Show, Eq, Num)
instance Component Countdown where type Storage Countdown = Unique Countdown 
```

## Displaying Noughts & Crosses

With those Global Components out of the way, we can focus on storing the actual noughts and crosses to draw on the screen. Normally, this would require us to create new Components to store their positions, drawing data, etc., however `apecs-sdl-gdk` exposes a nice way to capture this for minimal effort. `apecs-sdl-gdk` exposes a `Renderable` sum type and a `Position` component, which are both stored in a Map. The actual definitions of both are shown below, and I will go into more detail about what they do alongside those definitions.

```sourceCode haskell
data RenTexture = RenTexture
    { textureRef :: String
    -- ^ Identifier for the texture to render
    , textureLayer :: Int
    -- ^ layer for draw order
    , animationFrame :: Maybe Int
    -- ^ Frame index for animations, if applicable
    , textureVisible :: Bool
    } deriving (Eq, Show)

data RenText = RenText
    { fontRef :: String
    , displayText :: String
    , textColour :: TTF.Color
    , textLayer :: Int
    , textVisible :: Bool
    } deriving (Show, Eq)

data RenPoint = RenPoint
    { pointColour :: V4 Word8
    , pointLayer :: Int
    , pointVisible :: Bool
    } deriving (Show, Eq)

data RenLine = RenLine
    { lineColour :: V4 Word8
    , lineLayer :: Int
    , lineX :: Float -- ^ X coordinate of the line's ending point
    , lineY :: Float -- ^ Y coordinate of the line's ending point
    , lineVisible :: Bool
    } deriving (Show, Eq)

data RenRectangle = RenRectangle
    { rectSize :: V2 Float -- ^ Width and height of the rectangle
    , rectColour :: V4 Word8
    , rectLayer :: Int
    , rectVisible :: Bool
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
    , backgroundColor = V4 255 255 255 255
    , targetFPS = VSync
    , showFPS = Just "Roboto-Regular"
    }
```

Following `Config`, we also have these other `Global` Components

```sourceCode haskell
newtype Camera = Camera { camFunc :: V2 Float -> V2 Float }
instance Semigroup Camera where
    (Camera f1) <> (Camera f2) = Camera $ \pos -> f1 (f2 pos)
instance Monoid Camera where
    mempty = Camera id
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

`Camera` is used to store a function to apply at rendering time to offset all `Renderable` Entities. An example of when you may want to use this is to lock the game window to always be centre on your player. `Time` is used to store the total accumulated time of the game. `Renderer` is used to store the SDL rendering context, and similarly `Window` is used to store the SDL window context.

With all of our Components being defined, we need to create our Game World type. In Apecs, this works by doing `makeWorld "world" [..]`, however `apecs-sdl-gdk` exposes `makeWorld'`, which alongside your own Components, also initialises the game world to include the `apecs-sdl-gdk` specific components. Thus, we need to write the following:

```haskell
makeWorld' [''Turn, ''Board, ''MousePosition, ''MouseButtons, ''Countdown]
```

What this is doing is utilising [Template Haskell](https://wiki.haskell.org/Template_Haskell) to create a `World` data type at compile-time which includes all the necessary data class instances to define our game world within Apecs. If you want to see what this compiles to, you can do so [here](https://hackage.haskell.org/package/apecs-0.9.6/docs/Apecs.html#v:makeWorld).

Finally, I like to define a type synonym around our World to make our lives ever so slightly easier

```haskell
type System' a = System World a
```

## Game Logic

Now with our World created, we can now start implementing our naughts-and-crosses game logic. We will first start by defining our Game's configuration.

```haskell
config :: Config
config = defaultConfig { windowTitle = "Naughts and Crosses"
                       , windowDimensions = (600,600)
                       , showFPS = Nothing }
```

The only notable configuration here is the window dimensions. For the sake of this toy example, we will make the entire window the naughts and crosses board, which means our mouse position will always be in one of the 9 sections of the board.

Now we will start writing our first system!

```haskell
initialise :: System' ()
initialise = do
    let rly = RenLine { lineColour = V4 0 0 0 255, lineLayer = 0, lineX = 0, lineY = 600, lineVisible = True}
        rlx = rly { lineY = 0, lineX = 600}
    line1 <- newEntity (Line rly, Position (V2 200 0))
    line2 <- newEntity (Line rly, Position (V2 400 0))
    line3 <- newEntity (Line rlx, Position (V2 0 200))
    void $ newEntity (Line rlx, Position (V2 0 400))
```

`initialise`, as the name suggests, initialises what we need for our game world. Thanks to Apecs and us providing a `Monoid` instance, the initial state of all our `Global` stores is taken care of. Therefore, all we need to do is set up the visual game board by creating lines to be rendered. We make use of `apecs-sdl-gdk`'s `Renderable` Component as well as `Position` so that the rendering of the lines is completely handled for us by the rendering pipeline. The `Position` Component represent's the starting point of the line, and `lineX` and `lineY` represent the x and y offset respectively for the end point of the line. `newEntity` is Apecs' way of creating new entities within the game world with the given Components. It returns an Entity, which is just an integer, however in practice most of the time you won't need to interact with Entity values directly.

Now let's define our main entry point into the game logic, our `step` function which will be called every frame to update the game world

```haskell
step :: Float -> System' ()
step dt = do
    t <- get global
    case t of
        PlayerTurn -> stepPlayer dt
        AITurn -> stepAI dt
        CheckWinPlayer -> stepCheckWin dt AITurn
        CheckWinAI -> stepCheckWin dt PlayerTurn
        Win -> stepWin dt
        Reset -> stepReset dt
```

Whilst this is not much code, it is still doing a good amount and is worth focusing on, specifically the use of `get global`. `get` is one of the ways to get Component data from a store. In this case, we are wanting to access our `Turn` Component, which is in a Global Store. Since Global stores are able to be queried using *any* entity (which in turn technically means Global components are shared across *all* entities), we are able to pass in any integer value into get and it will yield the correct Component from the Global Store. However, for the sake of readability, Apecs exposes `global`, which is simply defined as -1, and so we will be using that.

After getting our Turn component, we then pattern match on its value to step accordingly. An important thing to note here is that Apecs' top-level functions for component management use type inference for determining which Store to access. Therefore, if the type of `t` was unable to be inferred, we would get an error. In our case, since we pattern match on it, GHC can infer it is a `Turn` type, however in cases were it cannot, you may need to add a type constraint: `t <- get global :: System' Turn`.

Before getitng to our turn-specific step functions, I want to introduce a helper to reduce the amount of code we will need to write.

```haskell
updateBoard :: Board -> Int -> Int -> BoardEntry -> System' ()
updateBoard (Board b) r c t = do
    let (l1,r1) = splitAt c b
        (l2,r2) = splitAt r (head r1)
        r2' = t : tail r2
        r2'' = l2 ++ r2'
        r1' = r2'' : tail r1
        b' = l1 ++ r1'
        x = 200 * fromIntegral r
        y = 200 * fromIntegral c
    case t of
        Naught -> do
           _ <- newEntity (Rectangle RenRectangle { rectSize = V2 100 100, rectColour = V4 0 0 0 255, rectLayer = 1, rectVisible = True }, Position (V2 (x+50) (y+50)))
           modify global $ \(_ :: Turn) -> CheckWinPlayer
        Cross -> do
            _ <-newEntity (Line RenLine { lineColour = V4 0 0 0 255, lineLayer = 1, lineX = 100, lineY = 100, lineVisible = True }, Position (V2 (x+50) (y+50)))
            _ <- newEntity (Line RenLine { lineColour = V4 0 0 0 255, lineLayer = 1, lineX = -100, lineY = 100, lineVisible = True}, Position (V2 (x + 150) (y+50)))
            modify global $ \(_ :: Turn) -> CheckWinAI
        Empty -> return ()
    modify global $ \(Board _) -> Board b'
```

`updateBoard` takes our current game board, a row and column index, and the tile we want to replace the existing one with, and replaces that tile whilst also creating new Entities with `Renderable` and `Position` components to make them visible. It also progresses our game state such that we check for a win afterwards.

Now let's implement our turn-specific step functions. We will go top-down in our implementations.

```haskell
stepPlayer :: Float -> System' ()
stepPlayer dt = do
    MousePosition (V2 mx my) <- get global
    MouseButtons mbs <- get global
    (Board b) <- get global
    let r = floor (mx / 200)
        c = floor (my / 200)
        t = (b !! c) !! r
    case t of
        Empty -> when (SDL.ButtonLeft `Set.member` mbs) $ updateBoard (Board b) r c Naught
        _ -> return ()
```

As an overview, `stepPlayer` gets our globally stored mouse position and currently pressed mouse buttons, accesses the current tile being hovered over by the mouse, and if the tile is empty and the left mouse button is currently pressed, then updates the board and switches the game's state to check the board for a potential win.

The new thing here is `modify`. `modify` is a function used to update a single Component for a single Entity. In our case, we use `modify` to update our Global store for both our current `Turn` and our `Board`. Recall that for Global stores, the actual entity does not matter, and we simply use `global` as an alias for -1.

```haskell
stepAI :: Float -> System' ()
stepAI dt = do
    (Board b) <- get global
    let getTile = do
            n <- randomRIO (0,8) :: IO Int
            let r = n `div` 3
                c = n `mod` 3
                r' = fromIntegral r
                c' = fromIntegral c
                t = (b !! c') !! r'
            case t of
                Empty -> return (r',c')
                _ -> getTile
    (r,c) <- liftIO getTile
    updateBoard (Board b) r c Cross
```

`stepAI` introduces nothing that we have not seen before; it randomly generates an integer for an unoccupied tile, and updates the board. If you want to make a more sophisticated AI, go ahead!

```haskell
stepCheckWin :: Float -> Turn -> System' ()
stepCheckWin dt t = do
    (Board b) <- get global
    let lines = [(0,1,2), (3,4,5), (6,7,8), (0,3,6), (1,4,7), (2,5,8), (0,4,8), (2,4,6)]
        checkLine (Board b) (n1,n2,n3) 
            | t1 == Naught && t2 == Naught && t3 == Naught = Just (n1,n2,n3)
            | t1 == Cross && t2 == Cross && t3 == Cross = Just (n1,n2,n3)
            | otherwise = Nothing
            where
                r1 = n1 `div` 3
                c1 = n1 `mod` 3
                r2 = n2 `div` 3
                c2 = n2 `mod` 3
                r3 = n3 `div` 3
                c3 = n3 `mod` 3
                t1 = (b !! c1) !! r1
                t2 = (b !! c2) !! r2
                t3 = (b !! c3) !! r3
        isFull = Empty `notElem` concat b
        line = foldl' (\acc l -> if isJust acc then acc else checkLine (Board b) l) Nothing lines
    case line of
        Just (n1,_,n3) -> do
            let r1 = n1 `div` 3
                c1 = n1 `mod` 3
                r3 = n3 `div` 3
                c3 = n3 `mod` 3
                c1' = fromIntegral c1
                c3' = fromIntegral c3
                r1' = fromIntegral r1
                r3' = fromIntegral r3
            if c1' == c3' then
                void $ newEntity (Line RenLine {lineColour = V4 255 0 0 255, 
                                                lineLayer = 1,
                                                lineX = 600,
                                                lineY = 0,
                                                lineVisible = True },
                                                Position (V2 0 (200 * c1' + 100)))
            else if r1' == r3' then
                void $ newEntity (Line RenLine {lineColour = V4 255 0 0 255, 
                                                lineLayer = 1,
                                                lineX = 0,
                                                lineY = 600,
                                                lineVisible = True },
                                                Position (V2 (200 * r1' + 100) 0))
            else if (c1' > c3') then
                void $ newEntity (Line RenLine {lineColour = V4 255 0 0 255, 
                                                lineLayer = 1,
                                                lineX = -600,
                                                lineY = 600,
                                                lineVisible = True },
                                                Position (V2 600 0))
            else
                void $ newEntity (Line RenLine {lineColour = V4 255 0 0 255, 
                                                lineLayer = 1,
                                                lineX = 600,
                                                lineY = 600,
                                                lineVisible = True },
                                                Position(V2 0 0))
            _ <- newEntity (Countdown 3.0)
            modify global $ \(_ :: Turn) -> Win
        Nothing -> if isFull then do
                _ <- newEntity (Countdown 3.0)
                modify global $ \(_ :: Turn) -> Win
            else
                modify global $ \(_ :: Turn) -> t
```

`stepCheckWin` checks all possible locations for a complete line, and if so then creates a new line Entity for it along starting a timer of 3 seconds. If the board is full and no one has a line, then we just progress to the `Win` state to simulate the cooldown before wiping the board. For those that are straight lines, we offset them to be in the centre of the square. It is important to note that the axes for SDL, and by extension `apecs-sdl-gdk` increase to the right and increase downward for the x and y axes respectively.

```haskell
stepWin :: Float -> System' ()
stepWin dt = cmapM $ \(Countdown t) -> if t < 0
    then do
        modify global $ \(_ :: Turn) -> Reset
        return $ Right $ Not @Countdown
    else return $ Left $ Countdown (t-dt) 
```

`stepWin` is where a lot of new Apecs concepts are introduced, and so we will focus a lot on this. `cmapM` is the first new Apecs concept. `cmap` is Apecs' form of `map`, and its functionality is dependent on the type of the function you provide.

For example, suppose we provide `cmap` a function of type `(Componment1,Component2) -> Component1`. `cmap` iterates over all entities that has the component on the left-hand side, and then writes to the component on the right-hand side. In our case, we have a tuple on the left-hand side, and in Apecs tuples of Components are considered a Component, and is an example of how Components can be composed together. What happens with tuples is that Apecs maps over all entities which have the first component, and then checks if those entities also has the subsequent components in the tuple. For our case, this means Apecs will get all Entities which have `Component1`, and then also check if they have `Component2` as well.

As a result of this, when composing together components, for performance concerns it is important to include the most uniquely identifying component as the first component. For example, suppose we have a `Player`, `Enemy`, and `Position` component. In our game, we have only one player, but multiple enemies, and all of them have a `Position` component. If we wrote a cmap as follows:

```sourceCode haskell
cmap $ \(Position _, Player) -> ...
```

That would be less efficient than

```sourceCode haskell
cmap $ \(Player, Position _) -> ...
```

Since Apecs would be checking all Entities with a Position if they were a player, rather than checking just a single entity with Player if it had a Position component.

For the case of `stepWin`, we actually use `cmapM`. Like with `map` and its monadic variant `mapM`, `cmapM` is Apecs' monadic variant of `cmap`, allowing for a monadic function to be passed. It is important to note that, when using a monadic function, you are working within a Monad. Therefore, you have the ability to introduce side effects. Whilst this can be helpful, such as in our case of progressing the game state, it is important to always be mindful of what you are doing within your code. Introducing side effects where they should not be is bad practice, and by doing so you will not only make your life harder when debugging, but also remove much of the benefit Haskell provides by being a pure functional language!

With all that out of the way, let's actually focus on the type of `stepWin`'s `cmapM`, since it also is something of interest. The type of our function is `Countdown -> System' (Either Countdown (Not Countdown))`. If we remove the `System'` Monad from the type, like how it would be for `cmap`, we would get the following `Countdown -> Either Countdown (Not Countdown)`. As mentioned before, from our type we are getting an Entity which has a `Countdown` component, and then returning `Either Countdown (Not Countdown)`, but what does `Either Countdown (Not Countdown)` actually mean for Apecs?

Let's start with `Either`. In Apecs, `Either a b` is used to represent two possible outcomes, captured by our `Left a` and `Right b` respectively. In the case of our function, we capture two possible outcomes: we either write to `Countdown` in the case of our `Left`, or we delete `Countdown` in the case of our `Right` thanks to `Not`.

`Not :: Not c` is used to delete something, and there are other ways, such as with `Maybe`, or by using `destroy`. We will showcase `Maybe` later.

It is important to note that, when destroying an Entity, make sure to specify **all** the Entity's components. Failing to do so will mean you have components floating around in memory, which not only causes a memory leak but may affect logic!

```haskell
stepReset :: Float -> System' ()
stepReset dt = do
    cmap $ \(_ :: Renderable, Position _) -> (Nothing :: Maybe (Renderable, Position))
    initialise
    modify global $ \(_ :: Board) -> mempty :: Board
    modify global $ \(_ :: Turn) -> PlayerTurn
```

`stepReset` simply deletes all entities with a `Renderable` component along with their `Position` component, and resets the board by using its `mempty`. Since this removes every Renderable entity, we then re-initialise the grid lines, and then give the player the turn to play. The way we delete components in `stepReset` is using `Maybe`. Since `Maybe` represents optionality, the `Just c` case results in writing the component `c` like normal, and `Nothing` results in it being deleted. However, this method is not ideal. This requires us to additionally read `Position`, which is unnecessary since we just want to delete it. Therefore, a better alternative would have been to use `Either` or `destroy`, however it shows the multiple ways in which things can be done.

As you have probably noticed, all our step functions take in a `Float` named `dt`, but never actually use it. Whilst in practice you would simply adjust their type to never take it as an input (which is the right thing to do!), I wanted to reinforce the idea of passing around `dt`. When running a game with `apecs-sdl-gdk`, `dt` is given to your top-level step function, and represents the amount of time elapsed between the previous frame and the current frame in milliseconds. Therefore, in any case where you need to have frame specific timings, you will need to make use of the input `dt`.

## Final Touches

Now that we have written all of the systems needed for our game logic, we have two things left to do. The first is writing an event handler. In `apecs-sdl-gdk`, events are polled for you, and then passed onto your event handler as `[SDL.EventPayload]`. Therefore, if you want to see all the possible events that are polled and sent for handling, please refer to the [SDL Documentation](https://hackage.haskell.org/package/sdl2-2.5.5.0/docs/SDL-Event.html#t:EventPayload). Since SDL's `EventPayload` makes use of Sum types and records for capturing all possible event payload, we will make use of top-level pattern matching for our event handler functions

```haskell
handlePayload :: [SDL.EventPayload] -> System' ()
handlePayload = mapM_ handleEvent

handleEvent :: SDL.EventPayload -> System' ()
handleEvent (SDL.MouseMotionEvent ev) = handleMouseMotionEvent ev
handleEvent (SDL.MouseButtonEvent ev) = handleMouseButtonEvent ev
handleEvent _ = return ()

handleMouseMotionEvent :: SDL.MouseMotionEventData -> System' ()
handleMouseMotionEvent ev = let
        (SDL.P pos) = SDL.mouseMotionEventPos ev
    in
        modify global $ \(MousePosition _ ) -> MousePosition $ fromIntegral <$> pos

handleMouseButtonEvent :: SDL.MouseButtonEventData -> System' ()
handleMouseButtonEvent ev
    | SDL.mouseButtonEventMotion ev == SDL.Pressed = modify global $ \(MouseButtons mbs) -> MouseButtons (Set.insert (SDL.mouseButtonEventButton ev) mbs)
    | SDL.mouseButtonEventMotion ev == SDL.Released = modify global $ \(MouseButtons mbs) -> MouseButtons (Set.delete (SDL.mouseButtonEventButton ev) mbs)
    | otherwise = return ()
```

Finally, the only thing left to do is to write our `main` function to start our game. As mentioned, `apecs-sdl-gdk` exposes 3 main functions to streamline this. `initialise`, `run`, and `draw`. 
- `initialise` is used to create the SDL window and renderer context, and needs your Apecs `World`
- `run` is the main game loop, which takes the entry point for stepping your game world, your event handler, and a draw function
- `draw` which is the default draw function, and handles the entire rendering pipeline for you by using all entities which have `Renderable` and `Position` components

```haskell
main :: IO ()
main = do
    w <- initWorld
    runWith w initialise
    (window, renderer) <- GDK.initialise w config
    run w renderer window step handlePayload draw
```

And just like that, our game is complete!

## Tips & Tricks for Haskell Game Development

With our naughts-and-crosses game complete, I now want to just highlight and collate together some tips for developing games, especially with Apecs, SDL, and `apecs-sdl-gdk`

- Always try write game logic as separated as possible. Not only is this good software practice in general, but for Haskell specifically, it is one of the best practices you can follow as it will allow for more reusable, idiomatic code through the use of e.g. higher order functions.
- When working with Apecs, since you are always inside a `System' a` Monad, it is very tempting to make use of functions like `cmapM`, `cfoldM`, etc. However, if you are able to write a Component logic which does not need to be inside a Monadic context, then you should not be inside one, as being inside one means you have the possiblity of introducing side effects.
- Interactable user interfaces are inherently stateful things. Therefore, when attempting to build one I recommend decomposing each UI element by exploiting Haskell's type system, and making use of functions being first-class citizens.
- SDL allows for a lot of changes to the renderer or window to be done pragmatically. Therefore, you are able to do pretty much anything you desire. However, the `sdl2` library binds onto the underlying C API for SDL2, and therefore has quite a bit of boilerplate and is very stateful
- When wanting to handle multiple different events, such as keyboard input, mouse movement, mouse buttons, etc. it is often quite useful having Globally stored data structures for storing the currently pressed buttons, like we did for `MouseButtons`, as it allows you to access them from anywhere within your game logic.
