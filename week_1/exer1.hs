main :: IO()
main = do
  print (doubleEveryOtherLeft [8,7,6,5])
  print (doubleEveryOtherLeft [1,2,3])
  print (sumDigits(doubleEveryOther(toDigitsRev 4012888888881881)))
  print (validate 4012888888881881)
  print (validate 4012888888881882)

toDigits :: Integer -> [Integer]
toDigits x
  | x > 0 = x`mod`10 : toDigits(x`div`10)
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

doubleEveryOtherLeft :: [Integer] -> [Integer]
doubleEveryOtherLeft [] = []
doubleEveryOtherLeft (x:[]) = [x]
doubleEveryOtherLeft (x:(h:t)) = x : 2*h : doubleEveryOtherLeft t

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . (doubleEveryOtherLeft . reverse)

sumDigits :: [Integer] -> Integer
sumDigits (h:[]) 
  | h < 10 = h
  | otherwise = h`div`10 + h`mod`10
sumDigits (h:t) 
  |h < 10 = h + sumDigits t
  | otherwise = h `div` 10 + sumDigits ((h`mod`10):t)

validate :: Integer -> Bool
validate x = ((sumDigits . (doubleEveryOther . toDigitsRev)) x) `mod`10 == 0




