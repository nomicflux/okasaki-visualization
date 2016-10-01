module Main where

import Prelude ((<>), id, show, pure, unit, Unit(), (>>=), bind, void)

-- import Structure.Stack as Stack
import Control.Monad.Eff (Eff)

import Graphics.D3.Base
import Graphics.D3.Util
import Graphics.D3.Selection
import Graphics.D3.Scale

arr :: Array Number
arr = [ 4.0, 8.0, 15.0, 16.0, 23.0, 42.0 ]

main :: forall eff. Eff (d3 :: D3 | eff) Unit
main = void do
  x <- linearScale
       .. domain [0.0, max' id arr]
       .. range [0.0, 420.0]
       .. toFunction
  rootSelect ".chart"
    .. selectAll "div" .. bindData arr
    .. enter .. append "div"
    .. style' "width" (\d -> show (x d) <> "px")
    .. text' show
