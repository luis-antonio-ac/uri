compareInTime :: (Int, Int) -> (Int, Int) -> (Int, Int)
compareInTime (at, bt) (a, b) 
    | a > b = (at + 1 , bt)
    | b > a = (at, bt + 1)
    | a == b = (at, bt)

compareLists :: [Int] -> [Int] -> (Int, Int) -> (Int, Int)
compareLists a b (at, bt) = foldl compareInTime (0, 0) $ zip a b