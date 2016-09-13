validityCheckSum :: [Integer] -> Integer
validityCheckSum [] = 0
validityCheckSum [x] = x
validityCheckSum (x:y:xs) = digitSum (y * 2) + x + validityCheckSum xs

digitSum :: Integer -> Integer
digitSum n = div + mod
    where (mod,div) = modulousDiv n

isValidCard :: String -> Bool
isValidCard x
    | length x == 16 = (validityCheckSum . convertIntoIntListAndReverse . convertStringIntoInteger) x `mod` 10 == 0
    | otherwise = error "Debit or Credit card should contain 16 digit"

convertStringIntoInteger :: String -> Integer
convertStringIntoInteger x = read x :: Integer

modulousDiv :: Integer -> (Integer,Integer)
modulousDiv n = (n `mod` 10, n `div` 10)

convertIntoIntListAndReverse :: Integer -> [Integer]
convertIntoIntListAndReverse 0 = []
convertIntoIntListAndReverse x = [mod] ++ convertIntoIntListAndReverse (div)
    where (mod,div) = modulousDiv x
