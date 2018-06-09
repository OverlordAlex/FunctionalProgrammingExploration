main = putStrLn "Solutions for project Eulers 1, 2, 4, 5, 6"

euler1 :: (Integral a) => a
euler1 = sum [x | x <- [1..999], mod x 3 == 0 || mod x 5 == 0 ]

fib :: Integral a => a -> a
fib n
    | n <= 1 = 1
    | otherwise = fib (n-1) + fib (n-2)
-- good lord this is slow
euler2 :: Integer
euler2 = sum [ x | x <- takeWhile (< 4000000) [fib x | x <- [1..]], even x]

isPalindrome :: Eq n => [n] -> Bool
isPalindrome [] = True 
isPalindrome n
    | (head n) == (last n) = True && if (length n > 1) then isPalindrome (init (tail n)) else True
    | otherwise = False

euler4 :: Integer
euler4 = maximum [a*b | a <- [100..999], b <- [a..999], isPalindrome (show (a*b))]

divs :: Integral a => a -> a -> Bool
divs n x
    | x == 1 = True
    | mod n x == 0 = divs n (x-1)
    | otherwise = False

-- this works, but good lord is it slow
euler5 :: Integer
euler5 = head [x | x <- [20..], divs x 20]

euler6 :: Integer
euler6 = sum [1..100] ^ 2 - sum [x^2 | x <- [1..100]] 

euler9 :: Integer
euler9 = head (map product [[a,b,c] | a <- [1..998], b <- [a..998], let c = 1000 - a - b, a^2 + b^2 == c^2])

divisors :: Integral a => a -> [a]
divisors n = [x | x <- [1..div n 2], mod n x == 0 ] ++ [n]
-- this works, but good lord is it slow..er. Its really slow
euler12 :: Integer
euler12 = head [n | n <- [ sum [1..x] | x <- [1..]], length (divisors n) > 500]

-- TODO - unfinished
collatz 1 = 1
collatz n
    | even n == True = 1 + collatz (n / 2)
    | otherwise = 1 + collatz (3*n + 1)
