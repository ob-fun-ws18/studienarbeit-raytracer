doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = 
    if x > 100
        then x
        else x*2
        
doubleSmallNumber' x = doubleSmallNumber x + 1
doubleMeList l = (head l) + 1

compareNumber :: (Ord a) => a -> a -> Ordering
compareNumber x y comparator = x comparator y

v3 = [1.0, 0.0, 0.0]
-- everything after $ has higher precedence than what comes before
-- dot a b = sum $ zipWith (*) a b
dot :: [Float] -> [Float] -> Float
dot a b = sum (zipWith (*) a b)

normalize :: [Float] -> [Float]
normalize a = [x / sqrt (dot a a) | x <- a]
v3add a b = zipWith (+) a b
v3sub a b = zipWith (-) a b

