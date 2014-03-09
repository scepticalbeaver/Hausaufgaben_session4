func:: Num a => a -> [a] -> [a]
func x xs = map (\y -> y*x) xs

--func' x = map (\y -> y*x) 

--func'' x = (map . (*)) x

funcRemake:: Num a => a -> [a] -> [a]
funcRemake = map . (*)