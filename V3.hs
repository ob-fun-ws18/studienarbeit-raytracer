data V3 a = V3 a a a deriving (Show)  
  
vadd :: (Floating t) => V3 t -> V3 t -> V3 t  
(V3 x y z) `vadd` (V3 x2 y2 z2) = V3 (x+x2) (y+y2) (z+z2)

vmult :: (Floating t) => t -> V3 t -> V3 t
(s) `vmult` (V3 x y z) = V3 (s*x) (s*y) (s*z)

vdiv :: (Floating t) => V3 t -> t -> V3 t
(V3 x y z) `vdiv` (s) = V3 (x/s) (y/s) (z/s)

dot :: (Floating t) => V3 t -> V3 t -> t
(V3 x y z) `dot` (V3 x2 y2 z2) = (x*x2 + y*y2 + z*z2)

vlength :: (Floating t) => V3 t -> t
vlength (V3 x y z) = (sqrt (dot (V3 x y z) (V3 x y z)))

normalize :: (Floating t) => V3 t -> V3 t
normalize (V3 x y z) = (V3 x y z) `vdiv` vlength (V3 x y z)

