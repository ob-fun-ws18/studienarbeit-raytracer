import System.IO
import Control.Monad
import Data.List.Split

import Vector

-- Sphere : x,y,z pos & radius
-- type Sphere = (V3, Float)
data Sphere = Sphere V3 Float

hitSphere :: V3 -> V3 -> Sphere -> Float
hitSphere rayOrigin rayDir (Sphere pos radius) =
    if (discriminant < 0)
        then -1.0
    else if root1 > 0.0
        then root1
    else (-b+droot) / 2.0
    where sphereToOrigin = rayOrigin `vsub` pos
          b = 2 * (rayDir `dot` sphereToOrigin)
          c = (sphereToOrigin `dot ` sphereToOrigin) - (radius * radius)
          discriminant = b*b - 4*c
          droot = sqrt discriminant
          root1 = (-b-droot) / 2.0    

-- camera setup
cameraPos = (V3 0 0 5)
up = (V3 0 1 0)
lookAt = (V3 0 0 0)
viewDir = normalize $ lookAt `vsub` cameraPos
viewUp = normalize $ up `vsub` ((dot up viewDir) `vmult` viewDir )
viewRight = viewDir `cross` viewUp

-- viewport setup
viewportWidth = 2.0
viewportHeight = 2.0
resX = 1024
resY = 1024
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
trace :: Float -> Float -> Sphere -> Float
trace gridX gridY sphere =
    hitSphere cameraPos rayDir sphere
    where
        rayDir = cmpRay (x(cmpVPpxl gridX gridY)) (y(cmpVPpxl gridX gridY))
       
       
distanceList = [trace x y (Sphere (V3 0 0 0) 2) | x <- [0..(resX-1)], y <- [0..(resY-1)]]

-- maps distances to RGB white tuple or RGB black tuples according to distance
toRGBTupleList :: [(Float, Float, Float)]
toRGBTupleList = map (\value -> if (value > -1.0) then (1.0, 1.0, 1.0) else (0.0, 0.0, 0.0)) distanceList

-- maps distances to Characters, '#' if hit, '_' otherwise
toStringList :: [String]
toStringList = map (\value -> if (value > -1.0) then "#" else "_") distanceList

-- yeah... does stuff I cannot read anymore and I forgot how it works
printGrid arr = mapM_ (putStrLn . unwords) $ map (map show) $ chunksOf 21 arr

-- convert color values to PPM format
makePPM :: Int -> Int-> [ (Float, Float, Float) ] -> String
makePPM width height xs = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n" ++ stringify(xs)
		  where stringify [] = ""
			stringify ((r,g,b):xs) = show (round (r*255)) ++ " " 
						 ++ show (round (g*255)) ++ " " 
						 ++ show (round (b*255)) ++ " "
						 ++ stringify xs
         

-- main = make_pgm resX resY toRGBTupleList
main = do writeFile "test.ppm" (makePPM (round resX) (round resY) toRGBTupleList)