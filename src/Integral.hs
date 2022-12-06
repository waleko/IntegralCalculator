{-# LANGUAGE InstanceSigs #-}

module Integral where

type Func = Double -> Double

data Bound a = Val a | PlusInfinity | MinusInfinity deriving (Show, Eq)

data IntegralError = Diverging | InvalidBounds deriving (Show, Eq)

data Bounds = Bounds
  { lower :: Bound Double,
    upper :: Bound Double
  } deriving (Show, Eq)

reverseBounds :: Bounds -> Bounds
reverseBounds (Bounds low up) = Bounds up low
checkBounds :: Bounds -> Either IntegralError ()
checkBounds (Bounds MinusInfinity MinusInfinity) = Left InvalidBounds
checkBounds (Bounds PlusInfinity PlusInfinity) = Left InvalidBounds
checkBounds _ = Right ()
data Partition = Partition {
  holder :: [Double],
  delta :: Double
} deriving (Show, Eq)

partition :: Bounds -> Int -> Partition
partition (Bounds (Val low) (Val up)) n = Partition [low + delta * fromIntegral idx | idx <- [0 .. (n - 1)]] delta
  where
      delta = (up - low) / fromIntegral n
partition (Bounds (Val low) PlusInfinity) n = partition (Bounds (Val low) (Val $ low + fromIntegral n)) n
partition (Bounds MinusInfinity (Val up)) n = partition (Bounds (Val $ up - fromIntegral n) (Val up)) n
partition (Bounds MinusInfinity PlusInfinity) n = partition (Bounds (Val $ - fromIntegral n) (Val $ fromIntegral n)) n
partition (Bounds MinusInfinity MinusInfinity) _ = Partition [] 0
partition (Bounds PlusInfinity PlusInfinity) _ = Partition [] 0
partition b n = Partition (reverse $ map (+delta) holder) (-delta)
  where
    (Partition holder delta) = partition (reverseBounds b) n

data Strategy = Rectangle | Trapezoid | Paraboloid deriving (Show, Eq)

newtype ResultWithSteps a = ResultWithSteps {resultWithSteps :: (a, Int)}
type FuncWithSteps = Double -> ResultWithSteps Double

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

instance Num a => Num (ResultWithSteps a) where
  (+) x1 x2 = (+) <$> x1 <*> x2
  (*) x1 x2 = (*) <$> x1 <*> x2
  abs = fmap abs
  signum = fmap signum
  fromInteger x = pure (fromInteger x)
  negate = fmap negate
instance Fractional a => Fractional (ResultWithSteps a) where
  fromRational x = pure (fromRational x)
  recip = fmap recip

unit :: a -> ResultWithSteps a
unit x = ResultWithSteps (x, 1)

meanFHelper :: Double -> Double -> Double -> Func -> FuncWithSteps
meanFHelper prev eps gamma f x = do
  y1 <- unit $ f (x - eps)
  y2 <- unit $ f (x + eps)
  let val = (y1 + y2) / 2
  let delta = abs (val - prev)
  if delta < gamma
    then return val
    else meanFHelper val (eps / 2) gamma f x

meanF :: Double -> Func -> FuncWithSteps
meanF gamma f x = do
  y0 <- unit $ f x
  if abs y0 /= 1/0 && y0 == y0 -- infinity and NaN check
    then return y0
    else meanFHelper (1/0) (gamma / 10) gamma f x

evalWithN :: Strategy -> Partition -> FuncWithSteps -> ResultWithSteps Double
evalWithN Rectangle (Partition xs delta) f = sum [f (x + delta / 2) | x <- xs] * pure delta
evalWithN Trapezoid (Partition xs delta) f = sum [(f x + f (x + delta)) / pure 2 | x <- xs] * pure delta
evalWithN Paraboloid bounds f = (4 * evalWithN Trapezoid bounds f - evalWithN Trapezoid bounds f) / pure 3

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

divergingThresholdN :: Int
divergingThresholdN = 100

eval :: IntegralProps -> Bounds -> Func -> Either IntegralError IntegralResult
eval (IntegralProps strategy maxError) bounds func = do
  let evaluator = evalHelper 0 0 1
  let (result, steps) = resultWithSteps evaluator
  _ <- checkBounds bounds
  case result of
    Left ie -> Left ie
    Right x -> Right $ IntegralResult x steps
  where
    evalHelper :: Double -> Double -> Int -> ResultWithSteps (Either IntegralError Double)
    evalHelper prev prevDelta n = do
      let partitioned = partition bounds n
      let gamma = delta partitioned / fromIntegral n / 4
      let wfunc = meanF gamma func
      val <- evalWithN strategy partitioned wfunc
      let delta = abs (val - prev)
      if delta < maxError && n >= 4
        then return (Right val)
        else
          if n >= divergingThresholdN && delta > prevDelta
            then return (Left Diverging)
            else evalHelper val delta (2 * n)
