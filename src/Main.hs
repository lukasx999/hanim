module Main (main) where

import Graphics.Gloss
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.IO.Unsafe

width = 900
height = 600

circ :: Float -> Float -> Float -> Picture
circ dt speed_factor radius =
    let w = fromIntegral width in
    -- circle starts at center
    let x = ((w / 2) - radius) * (sin $ dt * speed_factor) in
    Translate x 0 $ Color white $ Circle radius

draw :: Float -> Picture
draw dt =
    let c = circ dt 1 100 in
    let c' = circ dt 2 75 in
    let c'' = circ dt 3 50 in
    let c''' = circ dt 4 25 in
    Pictures [c, c', c'', c''']

main :: IO ()
main =
    let dpy = InWindow "functional graphics" (width, height) (0, 0) in
    animate dpy black draw
