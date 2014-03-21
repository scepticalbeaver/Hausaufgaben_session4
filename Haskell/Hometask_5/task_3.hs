module Task3 where

import Control.Monad

canonicFoo = flip replicateM [1..3]

myFoo n = sequence $ replicate n [1..3]


test = myFoo 3 == canonicFoo 3
