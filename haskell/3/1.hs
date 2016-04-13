f :: Double->Double
f x = 2 * x

diff :: (Double->Double)->Double->Double->Double
diff f p x = ((f(x + p) - f(x)) / p )

antidiff :: (Double->Double)->Double->Double->Double
antidiff f p x = ((f(x - p) + f(x)) * p )

integral :: (Double->Double)->Double->Double->Double->Double
integral f a b p = (antidiff f p b ) -( antidiff f p a)
