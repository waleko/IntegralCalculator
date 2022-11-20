module Evaluator where
import Control.Monad (msum)

type Func = Double -> Double

data Bounds = Bounds {
    lower :: Double,
    upper :: Double
}

data Strategy = Rectangle | Trapezoid | Paraboloid

evalWithN :: Strategy -> Bounds -> Func -> Int -> Double
evalWithN Rectangle (Bounds low up) f n = sum [f(x) | x <- xs] * delta
    where
        delta = (up - low) / fromIntegral (n)
        xs = [low + delta * (0.5 + fromIntegral idx) | idx <- [0..(n - 1)]]
-- TODO: improper + diverging
evalWithN Trapezoid (Bounds low up) f n = sum [(f(x) + f(x + delta)) / 2 | x <- xs] * delta
    where
        delta = (up - low) / fromIntegral (n)
        xs = [low + delta * (fromIntegral idx) | idx <- [0..(n - 1)]]

data IntegralProps = IntegralProps {
    strategy :: Strategy,
    maxError :: Double
}

data IntegralResult = IntegralResult {
    value :: Double,
    diverging :: Bool,
    steps :: Int,
    error :: Double
} deriving (Show, Eq)

eval :: IntegralProps -> Bounds -> Func -> IntegralResult
eval (IntegralProps strategy maxError) bounds func = evalHelper 0 1
    where evalHelper prev n = let val = evalWithN strategy bounds func n in if abs(val - prev) < maxError then (IntegralResult val False (2 * n - 1) (abs (val - prev))) else evalHelper val (2 * n)
