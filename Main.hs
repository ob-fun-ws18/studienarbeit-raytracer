import Vector

hitSphere :: V3 -> V3 -> V3 -> Float -> Float
hitSphere rayOrigin rayDir sphere radius =
    if (discriminant < 0)
        then -1.0
    else if root1 > 0.0
        then root1
    else (-b+droot) / 2.0
    where oc = rayOrigin `vsub` sphere
          b = 2 * (rayDir `dot` oc)
          c = (oc `dot ` oc) - (radius * radius)
          discriminant = b*b - 4*c
          droot = sqrt discriminant
          root1 = (-b-droot) / 2.0    

rayDir :: Float -> Float -> V3
rayDir resX resY =
    (V3 x y z)
    where x = 
          
trace :: Float -> Float -> [Float]
trace resX resY = 
    [y * resX + x | x <- [0..resX], y <- [0..resY]]
    where u = x / resX
          v = y / resY
          