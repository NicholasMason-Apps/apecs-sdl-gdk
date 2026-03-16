# apecs-sdl-gdk

`apecs-sdl-gdk` is a video game framework built on-top Apecs and SDL, providing tools to allow people to get up and running easily with `apecs` and `sdl2` for 2D game development in Haskell. More specifically, the package provides tools to minimise the boilerplate of SDL, and allow users to cleanly do rendering using a Type system. Additionally, since under the hood the package uses `sdl2`, users are able to freely control the window, renderer, etc. through utilising `sdl2`'s API.

## High-Level Overview

Below is a high-level overview of what `apecs-sdl-gdk` currently provides:
- Built-in rendering - through using a Type system and `apecs` Component system, users are able to avoid manually doing rendering calls in SDL by calling a single function to do it for them, allowing for layered rendering, camera manipulation, and more.
- Automated frame handling and running of game loop - `apecs-sdl-gdk` exposes a `run` function which handles frame deltas, exact time deltas, and more to accurately run your game loop to the desired frame rate, with frame-independent time deltas.
- Automated animation handling - textures simply need to be specified as an Animation, and `apecs-sdl-gdk` will handle the progressing of animations through sprite sheets for you, as well as the displaying of them during the rendering pipeline.
- Streamlined asset loading - textures and fonts can be loaded easily into corresponding maps, making use of the flyweight pattern for optimised memory usage, and reducing errors on your end.

## Links

Below are links to help people get familiar with the package and what it provides, as well as Apecs and SDL:
- [Documentation](URL)
- [Tutorial](https://github.com/NicholasMason-Apps/apecs-sdl-gdk/blob/main/examples/naughts-and-crosses.md)
- [Apecs hackage page](https://hackage.haskell.org/package/apecs)
- [SDL2 hackage page](https://hackage.haskell.org/package/sdl2)
- [SDL2 TTF hackage page](https://hackage.haskell.org/package/sdl2-ttf)
- [SDL2 Image hackage page](https://hackage.haskell.org/package/sdl2-image)