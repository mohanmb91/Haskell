ints2 :: [Int]
ints2 = [3,5..5999]

smallDivs :: Int -> [Int]
smallDivs n = [d | d <- takeWhile (<= n `div` 2) ints2, n `mod` d == 0]

primes :: [Int]
primes = 2:[p | p <- ints2, smallDivs p == []]

isPrime :: Int -> Bool
isPrime n = n `elem` primes

oddNonPrime :: [Int]
oddNonPrime = [d | d <- ints2, not (isPrime d) ]

isASquare :: Int -> Bool
isASquare number = n1 * n1 == number where n1 = floor $ sqrt $ (fromIntegral number::Double)

isNotInForm :: Int -> Bool
isNotInForm g = [p | p <- primes, isASquare ((g - p) `div` 2)] == []

finalList :: [Int]
finalList = [a | a <- oddNonPrime, isNotInForm a]
