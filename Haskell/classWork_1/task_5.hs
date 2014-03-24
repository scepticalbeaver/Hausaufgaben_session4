data Published =
                Book 
                { 
                    name :: String
                    , author      ::String
                    , cost :: Integer
                }
                | Magazine
                {
                    name :: String
                    , magNumber :: String
                    , cost :: Integer
                }

totalCost :: [Published] -> Integer
totalCost = foldr (\x  acc ->(cost x) + acc) 0
