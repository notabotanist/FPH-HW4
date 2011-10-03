module Matrix
where

type Matrix = [[Integer]]

add :: Matrix -> Matrix -> Matrix
add lm rm = zipWith (zipWith (+)) lm rm

sub :: Matrix -> Matrix -> Matrix
sub lm rm = zipWith (zipWith (-)) lm rm

transpose :: Matrix -> Matrix
transpose (m:[]) = [ [e] | e <- m ]
transpose (m:ms) = zipWith (++) (transpose (m:[])) (transpose ms)

cross :: (a -> b -> c) -> [a] -> [b] -> [[c]]
cross f l r = [ map (f le) r | le <- l ]

mul :: Matrix -> Matrix -> Matrix
mul l r = cross (\x y -> sum $ zipWith (*) x y) l (transpose r)
