module Main (main) where

import Graphics.Gloss
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.IO.Unsafe

title  = "the lambdas must flow!"
width  = 900
height = 600
colors = [ red, blue ]

circ :: Float -> Float -> Float -> Color -> Picture
circ dt speed_factor radius color =
    let w = fromIntegral width in
    -- circle starts at center
    let x = ((w / 2) - radius) * (sin $ dt * speed_factor) in
    Translate x 0 $ Color color $ Circle radius

circgen :: Float -> Float -> [Picture]
circgen dt n =
    let color = colors !! ((round dt) `mod` (length colors)) in
    map (\x -> circ dt (x * 0.1) (x * 5) color) [1..n]

draw :: Float -> Picture
draw dt =
    let circles = circgen dt 1000 in
    let pictures = [] in
    Pictures $ pictures ++ circles

main :: IO ()
main =
    let dpy = InWindow title (width, height) (0, 0) in
    animate dpy black draw
