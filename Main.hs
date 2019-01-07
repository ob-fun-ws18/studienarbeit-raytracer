import System.IO
import Control.Monad
import Data.List.Split
import Data.Maybe

import Vector

data Sphere = Sphere V3 Float V3 -- Position, Radius, Color
data HitRecord = HitRecord Float V3 V3 V3 -- Distance, Hitpoint, Normal, Color
minimumHitRecord :: [HitRecord] -> Maybe HitRecord
minimumHitRecord [] = Nothing
minimumHitRecord [hitrec] = Just hitrec
minimumHitRecord (x:xs) = if x `smallerThan` (minimumHitRecord xs) then Just x
                          else minimumHitRecord xs

smallerThan :: HitRecord -> Maybe HitRecord -> Bool
smallerThan _ Nothing = True
smallerThan (HitRecord distance _ _ _) (Just (HitRecord distance2 _ _ _)) = distance < distance2

distanceGreaterZero :: HitRecord -> Bool
distanceGreaterZero (HitRecord distance _ _ _) = distance > 0.0

distanceOfHitrecord :: Maybe HitRecord -> Maybe Float
distanceOfHitrecord Nothing = Nothing
distanceOfHitrecord (Just (HitRecord distance _ _ _)) = Just distance
               
colorOf :: Maybe HitRecord -> Maybe (Float, Float, Float)
colorOf Nothing = Nothing
colorOf (Just (HitRecord _ _ _ color)) = (Just (x(color), y(color), z(color)))

hitSphere :: V3 -> V3 -> Sphere -> HitRecord
hitSphere rayOrigin rayDir (Sphere pos radius color) =
    if (discriminant < 0)
        then (HitRecord 0.0 (V3 0 0 0) (V3 0 0 0) color) -- no hit
    else
        if root1 > 0.0
            then (HitRecord root1 hitPoint1 normal1 color)
        else (HitRecord ((-b+droot) / 2.0) hitPoint2 normal1 color)
    where sphereToOrigin = rayOrigin `vsub` pos
          b = 2 * (rayDir `dot` sphereToOrigin)
          c = (sphereToOrigin `dot ` sphereToOrigin) - (radius * radius)
          discriminant = b*b - 4*c
          droot = sqrt discriminant
          root1 = (-b-droot) / 2.0
          hitPoint1 = rayOrigin `vadd` (root1 `vmult` rayDir)
          hitPoint2 = rayOrigin `vadd` (droot `vmult` rayDir)
          normal1 = hitPoint1 `vsub` pos
          normal2 = hitPoint2 `vsub` pos

-- Generic hit function working on a list of hitable items (for now only Spheres)        
hit :: V3 -> V3 -> [Sphere] -> Maybe HitRecord
hit rayOrigin rayDir spheres =
    minimumHitRecord hitRecords
    where hitRecords = [ hitRecord | hitRecord <- [hitSphere rayOrigin rayDir sphere | sphere <- spheres], distanceGreaterZero hitRecord]

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
trace :: Float -> Float -> [Sphere] -> Maybe HitRecord
trace gridX gridY sphereList =
    hitrecord
    where
        rayDir = cmpRay (x(cmpVPpxl gridX gridY)) (y(cmpVPpxl gridX gridY))
        hitrecord = hit cameraPos rayDir sphereList
       
       
hitRecordList = [trace x y [(Sphere (V3 (-1) 0 0) 1 (V3 1 0 0)), (Sphere (V3 0.5 0 0) 1 (V3 0 1 0))] | x <- [0..(resX-1)], y <- [0..(resY-1)]]

-- maps distances to RGB white tuple or RGB black tuples according to distance
toRGBTupleList :: [Maybe HitRecord] -> [(Float, Float, Float)]
-- toRGBTupleList hitrecordList = [color | color <- [hitrec | hitrec <- colorOf hitrecordList, if isJust hitrec then (colorOf hitrec) else (0,0,0)]]
toRGBTupleList hitrecordList = map hitrecordToColor hitrecordList

hitrecordToColor Nothing = (0,0,0)
hitrecordToColor (Just (HitRecord _ _ _ (V3 x y z))) = (x, y, z)


-- maps distances to Characters, '#' if hit, '_' otherwise
-- toStringList :: [String]
-- toStringList = map (\value -> if isJust value then "#" else "_") hitRecordList

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
main = do writeFile "test.ppm" (makePPM (round resX) (round resY) (toRGBTupleList hitRecordList))