module Evaluator where
import Control.Monad (msum)

type Func = Double -> Double

data Bounds = Bounds {
    lower :: Double,
    upper :: Double
}

data Strategy = Linear | Trapezoid | Paraboloid

evalWithN :: Strategy -> Bounds -> Func -> Int -> Double
evalWithN Linear (Bounds low up) f n = sum [(f(x) + f(x + delta)) / 2 | x <- xs] * delta
    where
        delta = (up - low) / fromIntegral (n)
        xs = [low + delta * (fromIntegral idx) | idx <- [0..(n - 1)]]

stoppingCriteria :: Double
stoppingCriteria = 1e-8

eval :: Strategy -> Bounds -> Func -> Double
eval strategy bounds func = evalHelper 0 1
    where evalHelper prev n = let val = evalWithN strategy bounds func n in if abs(val - prev) < stoppingCriteria * abs(prev) then val else evalHelper val (n + 1)
