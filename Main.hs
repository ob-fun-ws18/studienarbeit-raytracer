import System.IO
import Control.Monad
import Data.List.Split
import Data.Maybe
import System.Random

import Vector

data Sphere = Sphere V3 Float V3 -- Position, Radius, Color
data HitRecord = HitRecord { distance :: Float, hitpoint :: V3, normal :: V3, colorOf :: V3 } -- Distance, Hitpoint, Normal, Color
minimumHitRecord :: [Maybe HitRecord] -> Maybe HitRecord
minimumHitRecord [] = Nothing
minimumHitRecord [Nothing] = Nothing
minimumHitRecord [Just a] = Just a
minimumHitRecord (Just x:xs) = if x `smallerThan` (minimumHitRecord xs) then Just x
                          else minimumHitRecord xs
minimumHitRecord (Nothing:xs) = minimumHitRecord xs

smallerThan :: HitRecord -> Maybe HitRecord -> Bool
smallerThan _ Nothing = True
smallerThan (HitRecord distance _ _ _) (Just (HitRecord distance2 _ _ _)) = distance < distance2

distanceGreaterZero :: Maybe HitRecord -> Bool
distanceGreaterZero (Just (HitRecord distance _ _ _)) = distance > 0.0
distanceGreaterZero Nothing = False

distanceOfHitrecord :: Maybe HitRecord -> Maybe Float
distanceOfHitrecord Nothing = Nothing
distanceOfHitrecord (Just (HitRecord distance _ _ _)) = Just distance
               
-- colorOf :: Maybe HitRecord -> Maybe (Float, Float, Float)
-- colorOf Nothing = Nothing
-- colorOf (Just (HitRecord _ _ _ color)) = (Just (x(color), y(color), z(color)))

data HasHit = HasHit Bool HitRecord

hitSphere :: V3 -> V3 -> Sphere -> Maybe HitRecord
hitSphere rayOrigin rayDir (Sphere pos radius color) =
    if (discriminant < 0)
        then Nothing -- no hit
    else
        if root1 > 0.0
            then Just (HitRecord root1 hitPoint1 normal1 color)
        else  Just (HitRecord ((-b+droot) / 2.0) hitPoint2 normal1 color)
    where sphereToOrigin = rayOrigin `vsub` pos
          b = 2 * (rayDir `dot` sphereToOrigin)
          c = (sphereToOrigin `dot ` sphereToOrigin) - (radius * radius)
          discriminant = b*b - 4*c
          droot = sqrt discriminant
          root1 = (-b-droot) / 2.0
          hitPoint1 = rayOrigin `vadd` ((root1-0.001) `smult` rayDir)
          hitPoint2 = rayOrigin `vadd` ((droot-0.001) `smult` rayDir)
          normal1 = hitPoint1 `vsub` pos
          normal2 = hitPoint2 `vsub` pos

    

reflect :: V3 -> V3 -> V3
reflect rayDirection normal = (-2) `smult` ((rayDirection `dot` normal) `smult` normal) `vadd` rayDirection
    

    --- Generic hit function working on a list of hitable items (for now only Spheres)
hit :: V3 -> V3 -> [Sphere] -> Maybe HitRecord
hit rayOrigin rayDir spheres =
    minimumHitRecord hitRecords
    where hitRecords = [ hitRecord | hitRecord <- [hitSphere rayOrigin rayDir sphere | sphere <- spheres], distanceGreaterZero hitRecord]

attenuate :: (Float, Float, Float) -> (Float,Float,Float) -> (Float,Float,Float)
attenuate (a,b,c) (x,y,z) = (a*x, b*y, c*z)

color :: V3 -> V3 -> [Sphere] -> Int -> (Float, Float, Float)
color _ _ _ 0 = (0.0,0.0,0.0)
color rayOrigin rayDirection spheres depth = 
    if isJust maybeHitRecord then
        attenuate (v3ToTuple . colorOf . fromJust $ maybeHitRecord) (color (hitpoint (fromJust maybeHitRecord)) (normalize (reflect rayDirection (normal (fromJust maybeHitRecord)))) spheres (depth-1))
    else (1,0.5*y(rayDirection)+0.5,1)
    where
        maybeHitRecord = hit rayOrigin rayDirection spheres


-- camera setup
cameraPos = (V3 0 0 20)
up = (V3 0 1 0)
lookAt = (V3 0 0 0)
viewDir = normalize $ lookAt `vsub` cameraPos
viewUp = normalize $ up `vsub` ((dot up viewDir) `smult` viewDir )
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
       centerToPixel = originToCenter `vadd` (vpX `smult` viewRight) `vadd` ((-vpY) `smult` viewUp)
        
-- returns distance from camera to hit or -1, if no hit
trace :: Float -> Float -> [Sphere] -> Int -> (Float, Float, Float)
trace gridX gridY sphereList depth =
    color cameraPos rayDir sphereList depth
    where
        rayDir = cmpRay (x(cmpVPpxl gridX gridY)) (y(cmpVPpxl gridX gridY))
       
       
randomSphereList :: Int -> IO [Sphere]
randomSphereList sphereCount = forM [1..sphereCount] $ \_ -> makeRandomSphere
    
makeRandomSphere = do
    colorR <- randomRIO (0.1, 1.0)
    colorG <- randomRIO (0.1, 1.0)
    colorB <- randomRIO (0.1, 1.0)
    
    radius <- randomRIO (0.1, 2.5)
    
    posX <- randomRIO (-10.0, 10.0)
    posY <- randomRIO (-10.0, 10.0)
    posZ <- randomRIO (-10.0, 10.0)
    
    return $ Sphere (V3 posX posY posZ) radius (V3 colorR colorG colorB)
        
defaultSpheres = [(Sphere (V3 (-1) 0 0) 1 (V3 1 1 1)), (Sphere (V3 1 0 0) 1 (V3 0.2 1 1))]
       
       
colors spheres depth = [trace x y spheres depth | x <- [0..(resX-1)], y <- [0..(resY-1)]]


colors' :: [Sphere] -> Int -> Int -> IO [(Float, Float, Float)]
colors' spheres sampleCount depth = let coordinates = [(x,y) | x <- [0..(resX-1)], y <- [0..(resY-1)]]
                  in forM coordinates $ \c -> traceSuper c spheres sampleCount depth

addTuples :: (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float, Float)
addTuples (a,b,c) (x,y,z) = (a+x, b+y, c+z)

divTupleBy :: Int -> (Float, Float, Float) -> (Float, Float, Float)
divTupleBy x (a, b, c) = (a/y, b/y, c/y) where y = fromIntegral x

traceSuper :: (Float, Float) -> [Sphere] -> Int -> Int -> IO (Float, Float, Float)
traceSuper (x, y) spheres sampleCount depth = do
    samples <- forM [1..sampleCount] $ \_ -> traceSuper' x y spheres depth
    return . divTupleBy sampleCount $ foldl addTuples (0.0,0.0,0.0) samples
    
traceSuper' :: Float -> Float -> [Sphere] -> Int -> IO (Float, Float, Float)
traceSuper' x y spheres depth = do
    xBias <- randomRIO (0.0, 1.0)
    yBias <- randomRIO (0.0, 1.0)
    return $ trace (x + xBias) (y + yBias) spheres depth
    

-- maps distances to Characters, '#' if hit, '_' otherwise
-- toStringList :: [String]
-- toStringList = map (\value -> if isJust value then "#" else "_") hitRecordList

-- yeah... does stuff I cannot read anymore and I forgot how it works
printGrid arr = mapM_ (putStrLn . unwords) $ map (map show) $ chunksOf 21 arr

-- convert color values to PPM format
makePPM :: Int -> Int-> [ (Float, Float, Float) ] -> String
makePPM width height xs = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n" ++ stringify(xs)
    where 
        stringify [] = ""
        stringify ((r,g,b):xs) = show (round (r*255)) ++ " " 
            ++ show (round (g*255)) ++ " " 
            ++ show (round (b*255)) ++ " "
            ++ stringify xs
         

-- main = make_pgm resX resY toRGBTupleList
main = do
    spheres <- randomSphereList 50
    colors'' <- colors' spheres 1 50
    writeFile "test.ppm" (makePPM (round resX) (round resY) colors'')