faca::Int->Int
faca 0 = 1
faca x = if x > 0 then x * faca(x-1) else x * faca(x * (-1) - 1)

facb::Int->Int
facb x
     | x == 0 = 1
     | x < 0 = x * facb(x * (-1) - 1)
     | otherwise = x * facb(x-1)
