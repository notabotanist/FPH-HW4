> module Main
> where

> import System.Environment

Slide 19 trial division implementation

> primes :: [Integer]
> primes = dsieve [2..]
>   where
>     dsieve (p:x) = p : dsieve [n | n <- x, n `mod` p > 0]

Tree-like Merging of Multiples
Algorithm found here: http://en.literateprograms.org/Sieve_of_Eratosthenes_(Haskell)#Tree-like_Merging_Of_Multiples

Basic operating principle is to generate a list of composite integers and
subtract it from the set of odd integers. The double primes feed "prevent[s]
retainment of unneeded primes at run-time."

> primesTME :: [Integer]
> primesTME = 2 : ([3,5..] `minus` foldt [[p*p,p*p+2*p..] | p <- primes'])
>   where
>     primes' = 3 : ([5,7..] `minus` foldt [[p*p,p*p+2*p..] | p <- primes'])
>     foldt ((x:xs):t)    = x : union xs (foldt (pairs t))
>     pairs ((x:xs):ys:t) = (x : union xs ys) : pairs t

Helper functions
Performs set functions on lists.

> minus :: (Ord t) => [t] -> [t] -> [t]
> minus (x:xs) (y:ys) = case (compare x y) of 
>   LT -> x : minus  xs  (y:ys)
>   EQ ->     minus  xs     ys 
>   GT ->     minus (x:xs)  ys
> minus  xs     _     = xs
> union :: (Ord t) => [t] -> [t] -> [t]
> union (x:xs) (y:ys) = case (compare x y) of 
>   LT -> x : union  xs  (y:ys)
>   EQ -> x : union  xs     ys 
>   GT -> y : union (x:xs)  ys
> union  xs     ys    = xs ++ ys

Main function

> main = do
>   args <- getArgs
>   let primeType = args!!0
>       primeIndex = read (args!!1) - 1
>   putStrLn . show $ (choosePrimeList primeType) !! primeIndex

> choosePrimeList :: String -> [Integer]
> choosePrimeList "sieve" = primes
> choosePrimeList "primes" = primesTME
> choosePrimeList _ = [1..]
