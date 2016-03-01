
--
discriminant::Double->Double->Double->Double
discriminant a b c = b^2 - 4 * a * c

nulpuntena::Double->Double->Double->[Double]
nulpuntena a b c = let d = discriminant a b c
  in if d < 0
  then []
  else if d == 0
    then [((-b) + sqrt(d)) / (2*a)]
    else [((-b) + sqrt(d)) / (2*a), ((-b) - sqrt(d)) / (2*a)]
--2b
nulpuntenb::Double->Double->Double->[Double]
nulpuntenb a b c
  | d < 0  = []
  | d == 0 = [((-b) + sqrt(d)) / (2*a)]
  | otherwise = [((-b) + sqrt(d)) / (2*a), ((-b) - sqrt(d)) / (2*a)]
  where d = b^2 - 4 * a * c
--2c
worpen5::Int
worpen5 = worpenN 5
--2d
worpenN::Int->Int
worpenN n = length [(a, b, c) | a <- [1..6], b <- [1..6], c <- [1..6], (a + b + c) `mod` n == 0 ]
