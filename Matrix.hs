module Matrix
where

add :: (Num a) => [[a]] -> [[a]] -> [[a]]
add lm rm = zipWith (zipWith (+)) lm rm

sub :: (Num a) => [[a]] -> [[a]] -> [[a]]
sub lm rm = zipWith (zipWith (-)) lm rm

transpose :: (Num a) => [[a]] -> [[a]]
transpose (m:[]) = [ [e] | e <- m ]
transpose (m:ms) = zipWith (++) (transpose (m:[])) (transpose ms)

cross :: (a -> b -> c) -> [a] -> [b] -> [[c]]
cross f l r = [ map (f le) r | le <- l ]

mul :: (Num a) => [[a]] -> [[a]] -> [[a]]
mul l r = cross (\x y -> sum $ zipWith (*) x y) l (transpose r)

--Basic element-by-element equality checking will be sufficient
data (Num a) => Matrix a = Matrix [[a]] deriving Eq
instance (Num a) => Show (Matrix a) where
--First convert each number into a string, then combine the individual
--strings into lines, and finally concatenate the lines into one string.
  show (Matrix x) = unlines . (map (unwords . (map show))) $ x

instance (Num a) => Num (Matrix a) where
  (Matrix l) + (Matrix r) = Matrix $ add l r
  (Matrix l) - (Matrix r) = Matrix $ sub l r
  (Matrix l) * (Matrix r) = Matrix $ mul l r
  abs a = a
  fromInteger i = Matrix [[(fromInteger i)]]
--signum is so defined so the law (abs x) * (signum x) == x holds
  signum (Matrix (x:xs)) = Matrix $ identity (length x)
 
identity :: (Num a) => Int -> [[a]]
identity x | x <= 1 = [[(fromInteger 1)]]
           | otherwise = ((fromInteger 1) : replicate (x-1) (fromInteger 0))
               : map (((fromInteger 0)):) (identity (x-1))
