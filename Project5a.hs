import Data.Char (digitToInt)

validityCheckSum :: [Integer] -> [Integer]
validityCheckSum = map (\(d, m) -> digitSum(d * m)) . pairs

pairs :: [Integer] -> [(Integer, Integer)]
pairs ds
 | odd (length ds) = zip ds (tail cycle12)
 | otherwise       = zip ds cycle12

digitSum :: Integer -> Integer
digitSum n = div + mod
    where (mod,div) = modulousDiv n

cycle12 :: [Integer]
cycle12 = cycle [1,2]

isValidCard :: String -> Bool
isValidCard x
    | length x == 16 = validityCheck x `mod` 10 == 0
    | otherwise = error "Debit or Credit card should contain 16 digit"

validityCheck = sum . validityCheckSum . reverse . toIntegerList

modulousDiv :: Integer -> (Integer,Integer)
modulousDiv n = (n `mod` 10, n `div` 10)

toIntegerList :: String -> [Integer]
toIntegerList =  map (fromIntegral . digitToInt)