module Vector
(
    V3(..),
    vadd,
    vmult,
    vdiv,
    dot,
    vlength,
    normalize
) where

data V3 = V3 { x :: Float, y :: Float, z :: Float } deriving (Show)  
  
vadd :: V3 -> V3 -> V3
(V3 x y z) `vadd` (V3 x2 y2 z2) = V3 (x+x2) (y+y2) (z+z2)

vmult :: Float -> V3 -> V3
(s) `vmult` (V3 x y z) = V3 (s*x) (s*y) (s*z)

vdiv ::  V3 -> Float -> V3
(V3 x y z) `vdiv` (s) = V3 (x/s) (y/s) (z/s)

dot :: V3 -> V3 -> Float
(V3 x y z) `dot` (V3 x2 y2 z2) = (x*x2 + y*y2 + z*z2)

vlength ::V3 -> Float
vlength (V3 x y z) = (sqrt (dot (V3 x y z) (V3 x y z)))

normalize :: V3 -> V3
normalize (V3 x y z) = (V3 x y z) `vdiv` vlength (V3 x y z)

