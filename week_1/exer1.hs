main :: IO()
main = do
  print (toDigits 123456789)
  print (toDigitsRev 123456789)
  print (doubleEveryOther (toDigitsRev 123456789))
  print (sumDigits (toDigits 1234))

toDigits :: Integer -> [Integer]
toDigits x
  | x > 0 = x`mod`10 : toDigits(x`div`10)
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

doubleEveryOtherLeft :: [Integer] -> [Integer]
doubleEveryOtherLeft (x:[]) = [x]
doubleEveryOtherLeft (x:(h:t)) = x : 2*h : doubleEveryOther t

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherLeft . reverse

sumDigits :: [Integer] -> Integer
sumDigits (h:[]) = h
sumDigits (h:t) = h + sumDigits t

validate :: Integer -> Bool
validate = sumDigits . doubleEveryOther . toDigits 
