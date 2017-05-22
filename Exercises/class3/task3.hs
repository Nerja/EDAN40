list1 = [(x,y) | x <- [1..], y <- [1..x]]
list2 = [1..] >>= (\x -> [1..x] >>= (\y -> return (x,y)))
