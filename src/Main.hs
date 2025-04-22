module Main (main) where

import Graphics.Gloss
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.IO.Unsafe

width = 900
height = 600

draw :: Float -> Picture
draw t =
    let w = fromIntegral width in
    let x = w * ((sin t) * 0.5 + 0.5) - w / 2 in
    let y = 0 in
    let circle = Translate x y $ Color white $ Circle 50 in
    Pictures [circle]

main :: IO ()
main =
    let dpy = InWindow "functional graphics" (width, height) (0, 0) in
    animate dpy black draw
