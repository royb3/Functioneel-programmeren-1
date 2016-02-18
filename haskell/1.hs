faca::Int->Int
faca 0 = 1
faca x = x * faca(x-1)

facb::Int->Int
facb x
     | x == 0 = 1
     | otherwise = x * facb(x-1)