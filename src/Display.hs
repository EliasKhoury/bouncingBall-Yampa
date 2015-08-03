module Display where

import           Control.Arrow              ((***))
import           Control.Monad
import           FRP.Yampa.VectorSpace
import           Graphics.UI.SDL            as SDL
import qualified Graphics.UI.SDL.Primitives as SDLP
import qualified Graphics.UI.SDL.TTF        as TTF
import           Text.Printf

--import Game
--import Display
import Input
import Constants
import Graphics.UI.Extra.SDL

type Pos = (Double, Double)

initializeDisplay :: IO ()
initializeDisplay = do
   -- Initialise SDL
  SDL.init [InitEverything]

initGraphs :: IO ()
initGraphs = do
  screen <- SDL.setVideoMode (round width) (round height) 32 [SWSurface]
  SDL.setCaption "Bounce" ""

  -- Important if we want the keyboard to work right (I don't know
  -- how to make it work otherwise)
  SDL.enableUnicode True

  -- Hide mouse
  SDL.showCursor True

  return ()

render :: Pos -> IO()
render pos = do
    -- Obtain surface
    screen <- getVideoSurface

    let format = surfaceGetPixelFormat screen
    bgColor <- mapRGB format 0x37 0x16 0xB4
    fillRect screen Nothing bgColor

    --displayInfo screen resources (gameInfo shownState)

    paintObject screen pos

    -- Double buffering
    SDL.flip screen

{-
-- * Painting functions
displayInfo :: Surface -> Resources -> GameInfo -> IO()
displayInfo screen resources over =
  printAlignRight screen resources
    ("Time: " ++ printf "%.3f" (gameTime over)) (10,50)
-}

paintObject :: Surface -> Pos -> IO()
paintObject screen pos = do
    _ <- SDLP.filledCircle screen (truncate $ fst pos) (truncate $ snd pos) ballSize (SDL.Pixel ballColor)
    return ()

{-

paintObject :: Surface -> Resources -> Object -> IO ()
paintObject screen resources object =
  case objectKind object of
    (Side {}) -> return ()
    _         -> do
      let (px,py)  = (\(u,v) -> (u, gameHeight - v)) (objectPos object)
      let (x,y)    = (round *** round) (px,py)
          (vx,vy)  = objectVel object
          (x',y')  = (round *** round) ((px,py) ^+^ (0.1 *^ (vx, -vy)))
      _ <- SDLP.filledCircle screen x y ballSize (SDL.Pixel ballColor)
      _ <- SDLP.line screen x y x' y' (SDL.Pixel velColor)
      return ()

-- * SDL Extensions
renderAlignRight :: Surface -> Surface -> (Int, Int) -> IO ()
renderAlignRight screen surface (x,y) = void $ do
  let rightMargin = SDL.surfaceGetWidth screen
      w           = SDL.surfaceGetWidth  surface
      h           = SDL.surfaceGetHeight surface
      rect        = SDL.Rect (rightMargin - x - w) y w h
  SDL.blitSurface surface Nothing screen (Just rect)
-}
