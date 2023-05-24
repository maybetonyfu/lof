data MyError = TooSmall | TooBig

checkNumber :: Int -> Either MyError Int
checkNumber x
  | x < 10 = Left TooSmall
  | x > 100 = Left  TooBig
  | otherwise = Right x

inputs = [1,2,5, 13, 60, 128]

processInput :: Int -> [Either MyError Int]
processInput inputs = 
  map checkNumber inputs

-- theme: basics
-- goanna results: 3
-- oracle: true
-- intended fix: 3