module Main (main) where

import Graphics.Gloss
import Data.Time.Clock.POSIX (getPOSIXTime)

title  = "the lambdas must flow!"
width  = 900
height = 600
colors = [ red, blue ]

circ :: Float -> Float -> Float -> Color -> Picture
circ dt speed radius color =
    -- circle starts at center
    let x   = (((fromIntegral width)  / 2) - radius) * (sin $ dt * speed) in
    let y   = (((fromIntegral height) / 2) - radius) * (sin $ dt * speed) in
    let rot = (round (dt * 100)) `mod` 360 in
    Rotate (fromIntegral rot)
        $ Translate x y
        $ Color color
        $ ThickCircle radius 5

circgen :: Float -> Float -> [Picture]
circgen dt n =
    let color = colors !! ((round dt) `mod` (length colors)) in
    map (\x -> circ dt (x * 0.1) (x * 5) color) [1..n]

draw :: Float -> Picture
draw dt =
    let circles = circgen dt 30 in
    let pictures = [] in
    Pictures $ pictures ++ circles

main :: IO ()
main =
    let dpy = InWindow title (width, height) (0, 0) in
    animate dpy black draw
