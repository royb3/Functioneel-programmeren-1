f :: Double->Double
f x = x^3

diff :: (Double->Double)->Double->Double->Double
diff f p x = ((f(x + p) - f(x)) / p )

integral :: (Double->Double)->Double->Double->Double
