module Main (main) where

import Graphics.Gloss
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.IO.Unsafe

width = 900
height = 600

draw :: Float -> Picture
draw dt =
    let radius = 50 in
    let w = fromIntegral width in
    -- circle starts at center
    let speed_factor = 5 in
    let x = ((w / 2) - radius) * (sin $ dt * speed_factor) in
    let circle = Translate x 0 $ Color white $ Circle radius in
    Pictures [circle]

main :: IO ()
main =
    let dpy = InWindow "functional graphics" (width, height) (0, 0) in
    animate dpy black draw
