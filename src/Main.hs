{-# LANGUAGE Arrows #-}

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.IORef
import FRP.Yampa as Yampa

import Display
import Input
import Graphics.UI.Extra.SDL


--type Pos = Double
type Vel = Double


-- This function is needed so I can return the controller to reactimate
mainGame :: Pos -> Vel -> SF Controller (Pos, Controller)
mainGame p v = proc ctrl -> do
                (p',_) <- game p v -< ctrl
                returnA -< (p',ctrl)

-- Handles mousclick (switches on mouseclick and moves ball)
game :: Pos -> Vel -> SF Controller (Pos, Vel)
game p v = switch (bb p v) (\(mouse, vel) -> bouncingBall mouse vel)
    where bb p' v' = proc ctrl -> do
                      (pos, vel) <- bouncingBall p' v' -< ctrl
                      event <- edge -< (controllerClick ctrl)
                      returnA -< ((pos, vel), event `tag` ((controllerPos ctrl),vel))

-- Regular ball falling
fallingBall :: Pos -> Vel -> SF Controller (Pos, Vel)
fallingBall y0 v0 = proc input -> do
    v <- integral >>^ (+ v0) -< 9.81
    p <- integral >>^ (^+^ y0) -< (0,v)
    returnA -< (p,v)

bouncingBall :: Pos -> Vel -> SF Controller (Pos, Vel)
bouncingBall y0 v0 = switch (bb y0 v0) (\(pos, vel) -> game pos (-vel * 0.8))
    where bb y0' v0' = proc input -> do
                        (pos, vel) <- fallingBall y0' v0' -< input
                        event <- edge -< snd pos >= 300  
                        returnA -< ((pos, vel), event `tag` (pos, vel))

main :: IO () 
main = do
    
    initializeDisplay -- From Display
    initGraphs -- From Display
    timeRef       <- initializeTimeRef
    controllerRef <- initializeInputDevices
    
    -- reactimate game loop takes: init input output sf
    reactimate (senseInput controllerRef)
        (\ _ -> do 
            mInput <- senseInput controllerRef
            dtSecs <- senseTime timeRef mInput
            return (if controllerPause mInput then 0 else (dtSecs * 10), Just mInput)
        )
        (\ _ (pos,c) -> do 
                        render pos
                        return (controllerExit c)
        ) 
        (mainGame (100.0,100.0) 0)

type MonadicT m a b = a -> m b 

senseTime :: IORef Int -> MonadicT IO Controller DTime
senseTime timeRef = \mInput ->
  let tt  = if controllerSlow mInput then (/10) else id
      tt1 = if controllerSuperSlow mInput then (/100) else tt
      tt2 = if controllerFast mInput then (*10) else tt1
  in fmap (tt2 . milisecsToSecs) $ senseTimeRef timeRef

