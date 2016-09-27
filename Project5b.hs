import Data.Char (digitToInt)
luhn = (0 ==) . (`mod` 10) . sum . map (\x -> (x `div` 10 + x `mod` 10)) .
       zipWith (*) (cycle [1,2]) . map digitToInt . reverse



--luhn = (0 ==) . (`mod` 10) . sum . map (uncurry (+) . (`divMod` 10)) .
--       zipWith (*) (cycle [1,2]) . map digitToInt . 
