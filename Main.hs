import System.IO
import Control.Monad
import Data.List.Split

import Vector

hitSphere :: V3 -> V3 -> V3 -> Float -> Float
hitSphere rayOrigin rayDir sphere radius =
    if (discriminant < 0)
        then -1.0
    else if root1 > 0.0
        then root1
    else (-b+droot) / 2.0
    where sphereToOrigin = rayOrigin `vsub` sphere
          b = 2 * (rayDir `dot` sphereToOrigin)
          c = (sphereToOrigin `dot ` sphereToOrigin) - (radius * radius)
          discriminant = b*b - 4*c
          droot = sqrt discriminant
          root1 = (-b-droot) / 2.0    

-- camera setup
cameraPos = (V3 5 0 5)
up = (V3 0 1 0)
lookAt = (V3 5 0 0)
viewDir = normalize $ lookAt `vsub` cameraPos
viewUp = normalize $ up `vsub` ((dot up viewDir) `vmult` viewDir )
viewRight = viewDir `cross` viewUp

-- viewport setup
viewportWidth = 2.0
viewportHeight = 2.0
resX = 21
resY = 21
pixelWidth = viewportWidth / resX
pixelHeight = viewportHeight / resY
stepX = pixelWidth / 2.0
stepY = pixelHeight / 2.0

-- compute viewport coordinates for given row, col in grid
cmpVPpxl :: Float -> Float -> V3
cmpVPpxl row col =
    (V3 x y z)
    where
        x = (pixelWidth * col) -1.0 + stepX
        y = (pixelHeight * row) -1.0 + stepY
        z = 0.0 -- don't really need this component

-- compute ray from camera to given viewport coordinate vpX, vpY
cmpRay :: Float -> Float -> V3
cmpRay vpX vpY = 
   normalize $ centerToPixel `vsub` cameraPos
   where
       originToCenter = cameraPos `vadd` viewDir
       centerToPixel = originToCenter `vadd` (vpX `vmult` viewRight) `vadd` ((-vpY) `vmult` viewUp)
        
-- returns distance from camera to hit or -1, if no hit
trace :: Float -> Float -> Float
trace gridX gridY =
    hitSphere cameraPos rayDir (V3 0 0 0) 2
    where
        rayDir = cmpRay (x(cmpVPpxl gridX gridY)) (y(cmpVPpxl gridX gridY))
       
       
distanceList = [trace x y | x <- [0..(resX-1)], y <- [0..(resY-1)]]

-- maps distances to RGB white tuple or RGB black tuples according to distance
toRGB888 = map (\value -> if (value > -1.0) then (255, 255, 255) else (0, 0, 0)) distanceList

toChar = map (\value -> if (value > -1.0) then "#" else "_") distanceList

-- yeah...
printGrid arr = mapM_ (putStrLn . unwords) $ map (map show) $ chunksOf 21 arr

main = printGrid toChar