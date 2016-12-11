module Views.Animation where

import Data.Maybe (Maybe(..))
import Math (sin, pi)
import Prelude ((*), (-), (/), (<))
import Signal.Time (Time, millisecond)

type Animation = { animationPhase :: Number
                 , delay :: Number
                 , startAnimation :: Maybe Time
                 }

data AnimationAction = Tick Time
                     | StartTimer Time

defaultAnimation :: Animation
defaultAnimation = { animationPhase : 1.0
                   , delay : 750.0 * millisecond
                   , startAnimation : Nothing
                   }

resetAnimation :: Animation -> Animation
resetAnimation anim = anim { animationPhase = 0.0 }

getPhase :: Animation -> Number
getPhase anim = anim.animationPhase

updateAnimation :: AnimationAction -> Animation -> Animation
updateAnimation (StartTimer time) anim =
  anim { startAnimation = Just time
       , animationPhase = 0.0
       }
updateAnimation (Tick time) anim =
  case anim.startAnimation of
    Nothing -> anim
    Just start ->
      let
        timeDiff = time - start
      in
       if anim.delay < timeDiff
       then anim { animationPhase = 1.0 }
       else anim { animationPhase = sin (timeDiff / anim.delay * pi / 2.0) }
