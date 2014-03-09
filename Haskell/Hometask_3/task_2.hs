listCorrect = zipWith(^) [2, 2 ..] [1..]


list = zipWith(+) (1 : list) (1 : list)


test = take 10 list == take 10 listCorrect