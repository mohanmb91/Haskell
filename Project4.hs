-- 																		TAKING Int AS AN PARAMETER

validityCheckSum :: (Integral a) => [a] -> a
validityCheckSum [] = 0
validityCheckSum [x] = x
validityCheckSum (x:y:xs) = digitSum (y * 2) + x + validityCheckSum xs
--validityCheckSum (x:y:xs) = [x, y*2] ++ validityCheckSum xs 


digitSum :: (Integral a) => a -> a
digitSum n = n `div` 10 + n `mod` 10 
             
isValidCard :: (Integral a) => a -> Bool
isValidCard x
    | lengthCheck' x == 16 = validityCheckSum(convertIntoIntList x) `mod` 10 == 0

convertIntoIntList :: (Integral a ) => a -> [a]
convertIntoIntList 0 = []
convertIntoIntList x = [x `mod` 10] ++ convertIntoIntList (x `div` 10) 

lengthCheck' ::(Integral a) => a -> a
lengthCheck' 0 = 0
lengthCheck' x = 1 + lengthCheck' (x `div` 10)

-- 																		TAKING STRING AS AN PARAMETER


--validityCheckSum :: (Integral a) => [a] -> a
--validityCheckSum [] = 0
--validityCheckSum [x] = x
--validityCheckSum (x:y:xs) = digitSum (y * 2) + x + validityCheckSum xs
----validityCheckSum (x:y:xs) = [x, y*2] ++ validityCheckSum xs 


--digitSum :: (Integral a) => a -> a
--digitSum n = n `div` 10 + n `mod` 10 


--isValidCard :: String -> Bool
--isValidCard "" = False
--isValidCard (x:xs)
--    | length (x:xs) == 16 = validityCheckSum (convertIntoIntList (convertStringIntoInteger (x:xs))) `mod` 10 == 0

--convertStringIntoInteger :: String -> Integer
--convertStringIntoInteger x = read x :: Integer


--convertIntoIntList :: Integer -> [Integer]
--convertIntoIntList 0 = []
--convertIntoIntList x = [x `mod` 10] ++ convertIntoIntList (x `div` 10) 


-- 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6  <== Input
-- 
-- 2 2 6 4 1 6 5 8 9 0 2 2 6 4 1 6  <== Excepted Output
