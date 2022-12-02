{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Lib where

type Func = Double -> Double

data Bounds = Bounds
  { lower :: Double,
    upper :: Double
  }

data Strategy = Rectangle | Trapezoid | Paraboloid deriving (Show, Eq)

newtype ResultWithSteps a = ResultWithSteps { resultWithSteps :: (a, Int) }
instance Functor ResultWithSteps where
  fmap :: (a -> b) -> ResultWithSteps a -> ResultWithSteps b
  fmap f (ResultWithSteps m) = ResultWithSteps $ let (y, steps) = m in (f y, steps)
instance Applicative ResultWithSteps where
  pure :: a -> ResultWithSteps a
  pure x = ResultWithSteps (x, 0)
  (<*>) :: ResultWithSteps (a -> b) -> ResultWithSteps a -> ResultWithSteps b
  (<*>) (ResultWithSteps transform) (ResultWithSteps m1) = ResultWithSteps $ let (f, steps0) = transform in let (y, steps1) = m1 in (f y, steps0 + steps1)
instance Monad ResultWithSteps where
  (>>=) :: ResultWithSteps a -> (a -> ResultWithSteps b) -> ResultWithSteps b
  (>>=) (ResultWithSteps m1) k = ResultWithSteps $ let (inter, steps1) = m1 in let (ResultWithSteps (res, steps2)) = k inter in (res, steps1 + steps2)
unit :: a -> Int -> ResultWithSteps a
unit x steps = ResultWithSteps (x, steps)

-- -- TODO: improper + diverging
evalWithN :: Strategy -> Bounds -> Func -> Int -> ResultWithSteps Double
evalWithN Rectangle (Bounds low up) f n = unit (sum [f x | x <- xs] * delta) n
  where
    delta = (up - low) / fromIntegral n
    xs = [low + delta * (0.5 + fromIntegral idx) | idx <- [0 .. (n - 1)]]
evalWithN Trapezoid (Bounds low up) f n = unit (sum [(f x + f (x + delta)) / 2 | x <- xs] * delta) n
  where
    delta = (up - low) / fromIntegral n
    xs = [low + delta * fromIntegral idx | idx <- [0 .. (n - 1)]]
evalWithN Paraboloid bounds f n = do
    x <- evalWithN Trapezoid bounds f (2 * n)
    y <- evalWithN Trapezoid bounds f n
    return $ (4 * x - y) / 3 

data IntegralProps = IntegralProps
  { strategy :: Strategy,
    maxError :: Double
  }
  deriving (Show, Eq)

data IntegralResult = IntegralResult
  { value :: Double,
    steps :: Int
  }
  deriving (Show, Eq)

data IntegralError = Diverging deriving (Show, Eq)

eval :: IntegralProps -> Bounds -> Func -> Either IntegralError IntegralResult
eval (IntegralProps strategy maxError) bounds func = do
    let evaluator = evalHelper 0 1
    let (result, steps) = resultWithSteps evaluator
    case result of
      Left ie -> Left ie
      Right x -> Right $ IntegralResult x steps
  where
    evalHelper :: Double -> Int -> ResultWithSteps (Either IntegralError Double)
    evalHelper prev n = do
      val <- evalWithN strategy bounds func n
      if abs (val - prev) < maxError then return (Right val) else evalHelper val (2 * n)