main :: IO()
main = do
  print (toDigits 1234)


toDigits :: Integer -> [Integer]
toDigits x
  | x > 0 = x`mod`10 : toDigits(x`div`10)
  | otherwise = []
