--1A
euclid::Integer->Integer->Integer
euclid x y
  |x > y = euclid (x - y) y
  |y > x = euclid x (y - x)
  |otherwise = x
--1B
egcd :: Integer -> Integer -> (Integer,Integer,Integer)
egcd 0 b = (b, 0, 1)
egcd a b =
   let (g, s, t) = egcd (b `mod` a) a
    in (g, t - (b `div` a) * s, s)

p = 7
q = 11
m = p * q
m' = (p-1) * (q-1)

--e::Integer->Integer
--e
--  |gcd(e, m') == 1 and e < m'

eigenEgcd :: Integer -> Integer -> (Integer, Integer, Integer)
eigenEgcd 0 b = (b, 0, 1)
eigenEgcd a b =
  let
    u(g, s, t) = egcd(b `mod` a) a
    middelsste = t - (b `div` a) * s
  in (g, if middelsste < 0 then (middelsste + b) else middelsste, s)
