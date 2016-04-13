import Data.Ord
import Data.Char
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

p = 101
q = 113
m = p * q
m' = (p-1) * (q-1)

--e::Integer->Integer
--e
--  |gcd(e, m') == 1 and e < m'

eigenEgcd :: Integer -> Integer -> (Integer, Integer, Integer)
eigenEgcd 0 b = (b, 0, 1)
eigenEgcd a b =
  let
    (g, s, t) = egcd(b `mod` a) a
    middelsste = t - (b `div` a) * s
    in (g, if middelsste < 0 then (middelsste + m) else middelsste, s)
mogelijkeGetallenVoorE = [ e | e <- [0..m'-1], (gcd e m') == 1]
mogelijkeGetallenVoorD e = [ d | d <- [0..40000] ,(e * d) `mod` m' == 1 ]

privateKey = 241
publicKey = 7761



rsaencrypt::(Integer,Integer)->Integer->Integer
rsaencrypt (key,modulus) c = (c ^ key) `mod` modulus

rsaencryptChar::(Integer, Integer)->Char->Integer
rsaencryptChar (key,modulus) char = let c = toInteger $ord char in (c ^ key) `mod` modulus


rsadecrypt::(Integer,Integer)->Integer->Integer
rsadecrypt (key,modulus) c = (c ^ key) `mod` modulus

rsadecryptChar::(Integer, Integer)->Integer->Char
rsadecryptChar (key,modulus) c = chr $fromInteger $(c ^ key) `mod` modulus

--5: Alice versleuteld de berichten voor Bob doormiddel van Bob's public key. Zo kan alleen bob deze weer decrypten. :)
