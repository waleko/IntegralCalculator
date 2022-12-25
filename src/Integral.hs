{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Integral where

-- | Integrable function type
type Func = Double -> Double

-- | Integral bound
data Bound a = Val a | PlusInfinity | MinusInfinity deriving (Show, Eq)

-- | Possible errors encountered during calculations
data IntegralError = Diverging | InvalidBounds deriving (Show, Eq)

-- | Integral bounds (possibly infinite)
data Bounds = Bounds
  { lower :: Bound Double,
    upper :: Bound Double
  }
  deriving (Show, Eq)

data Partition = Partition
  { holder :: [Double],
    step :: Double
  }
  deriving (Show, Eq)

-- | Devide a segment on a real line into equal segments
partition :: (Double, Double) -> Int -> Partition
partition (low, up) n = Partition [low + delta * fromIntegral idx | idx <- [0 .. (n - 1)]] delta
  where
    delta = (up - low) / fromIntegral n

-- | Method of computing the integral
data Strategy = Rectangle | Trapezoid | Paraboloid deriving (Show, Eq)

-- | Value with number of steps remaining
data ValueWithSteps a = ValueWithSteps {valueWithStepsLeft :: (a, Int)} | CancelledValue deriving (Show, Eq)

-- | Function that gets a result with the given maximum number of steps
-- | If execution exceeds this number, the value is set as `CancelledValue` and further code is executed
newtype ResultWithSteps a = ResultWithSteps {runWithLimit :: Int -> ValueWithSteps a}

type FuncWithSteps = Double -> ResultWithSteps Double

instance Functor ValueWithSteps where
  fmap :: (a -> b) -> ValueWithSteps a -> ValueWithSteps b
  fmap f (ValueWithSteps (y, steps0)) = ValueWithSteps (f y, steps0)
  fmap _ CancelledValue = CancelledValue

instance Functor ResultWithSteps where
  fmap :: (a -> b) -> ResultWithSteps a -> ResultWithSteps b
  fmap f (ResultWithSteps m) = ResultWithSteps $ \lim -> fmap f (m lim)

instance Applicative ResultWithSteps where
  pure :: a -> ResultWithSteps a
  pure x = ResultWithSteps $ \lim -> ValueWithSteps (x, lim)
  (<*>) :: ResultWithSteps (a -> b) -> ResultWithSteps a -> ResultWithSteps b
  (<*>) (ResultWithSteps transform) (ResultWithSteps m1) = ResultWithSteps $
    \lim -> case transform lim of
      ValueWithSteps (trFunc, remainingSteps) -> trFunc <$> m1 remainingSteps
      CancelledValue -> CancelledValue

instance Monad ResultWithSteps where
  (>>=) :: ResultWithSteps a -> (a -> ResultWithSteps b) -> ResultWithSteps b
  (>>=) (ResultWithSteps m1) k = ResultWithSteps $ \lim -> case m1 lim of
    ValueWithSteps (val, remainingSteps) -> runWithLimit (k val) remainingSteps
    CancelledValue -> CancelledValue


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

instance Num a => Num (Either IntegralError a) where
  (+) x1 x2 = (+) <$> x1 <*> x2
  (*) x1 x2 = (*) <$> x1 <*> x2
  abs = fmap abs
  signum = fmap signum
  fromInteger x = pure (fromInteger x)
  negate = fmap negate

-- Value with a single step required to compute it
unit :: a -> ResultWithSteps a
unit x = ResultWithSteps $ \lim -> if lim >= 1 then ValueWithSteps (x, lim - 1) else CancelledValue

-- | Approximate function value if its value is not defined
infinityHelper :: Double -> Func -> FuncWithSteps
infinityHelper delta f x = do
  -- offset
  let eps = delta / 10 -- no theoretical explanation, why it's 10
  return $ (f (x - eps) + f (x + eps)) / 2

-- | Fixes breakpoints of an integrable function
wrapperFunc :: (Double, Double) -> Double -> Func -> FuncWithSteps
wrapperFunc (low, up) delta f x = do
  y0 <- unit $ f x
  -- check from what side to approximate linearly: factor=1 means from the right, factor=-1 – from the left
  let center = (low + up) / 2
  let factor = if x < center then 1 else (-1)
  -- calculate value
  let res
        | abs y0 /= 1 / 0 && y0 == y0 = return y0 -- infinity and NaN check
        | y0 == low || y0 == up = return $ 2 * f (x + delta * fromIntegral factor) - f (x + delta * 2 * fromIntegral factor)
        | otherwise = infinityHelper delta f x
  res

-- | Common helper function for Rectangle and Trapezoid strategies
evalWithNHelper :: (FuncWithSteps -> ([Double], Double) -> ResultWithSteps Double) -> (Double, Double) -> Func -> Int -> ResultWithSteps Double
evalWithNHelper logic segment func n = do
  -- get partition
  let partitioned = partition segment n
  let xs = holder partitioned
  let delta = step partitioned
  -- get function without breakpoints
  let wfunc = wrapperFunc segment delta func
  -- calculate
  logic wfunc (xs, delta)

-- | Calculates the integral using the given strategy and the number of segments in the partition
evalWithN :: Strategy -> (Double, Double) -> Func -> Int -> ResultWithSteps Double
evalWithN Rectangle segment func n = evalWithNHelper (\wfunc (xs, delta) -> sum [wfunc (x + delta / 2) | x <- xs] * pure delta) segment func n
evalWithN Trapezoid segment func n = evalWithNHelper (\wfunc (xs, delta) -> sum [(wfunc x + wfunc (x + delta)) / pure 2 | x <- xs] * pure delta) segment func n
evalWithN Paraboloid segment func n = (4 * evalWithN Trapezoid segment func (2 * n) - evalWithN Trapezoid segment func n) / pure 3

-- | Integral calculation properties
data IntegralProps = IntegralProps
  { strategy :: Strategy,
    maxError :: Double,
    maxSteps :: Int
  }
  deriving (Show, Eq)

-- | Result of the integral calculation
data IntegralResult = IntegralResult
  { value :: Double,
    steps :: Int
  }
  deriving (Show, Eq)

type EvaluationResult = Either IntegralError IntegralResult

-- | Calculate the integral
eval :: IntegralProps -> Bounds -> Func -> EvaluationResult
eval props bounds func = do
  res <- evalMonad props bounds func
  let stepsLimit = maxSteps props
  case runWithLimit res stepsLimit of
    ValueWithSteps (val, stepsRemaining) -> return $ IntegralResult val (stepsLimit - stepsRemaining)
    CancelledValue -> Left Diverging

evalMonad :: IntegralProps -> Bounds -> Func -> Either IntegralError (ResultWithSteps Double)
evalMonad props (Bounds (Val low) (Val up)) func = Right $ evalSegment props (low, up) func
evalMonad props (Bounds (Val low) PlusInfinity) func = Right $ do
  -- I = \int_{low}^{+\infty} f
  let g = func . (\x -> x + low - 1) -- I = \int_1^{+\infty} g
  evalSegment props (0, 1) (\x -> g (1 / x) / (x * x)) -- I = \int_0^1 g(1/x) / x^2
evalMonad props (Bounds MinusInfinity (Val up)) func = evalMonad props (Bounds (Val (- up)) PlusInfinity) func
evalMonad (IntegralProps strategy maxError maxSteps) (Bounds MinusInfinity PlusInfinity) func = Right $ do
  -- I = \int_{-\infty}^{+\infty} f
  let halfProps = IntegralProps strategy (maxError / 2) maxSteps
  inside <- evalSegment halfProps (-1, 1) func
  outside <- evalSegment halfProps (-1, 1) (\x -> func (1 / x) / (x * x))
  return $ inside + outside
evalMonad _ (Bounds MinusInfinity MinusInfinity) _ = Left InvalidBounds
evalMonad _ (Bounds PlusInfinity PlusInfinity) _ = Left InvalidBounds
evalMonad props (Bounds l r) func = negate $ evalMonad props (Bounds r l) func

minimumDecisionN :: Int
minimumDecisionN = 5

-- Calculates the integral on a segment
evalSegment :: IntegralProps -> (Double, Double) -> Func -> ResultWithSteps Double
evalSegment (IntegralProps strategy maxError _) (low, up) func = do
  evalHelper 0 1
  where
    evalHelper :: Double -> Int -> ResultWithSteps Double
    evalHelper prev n = do
      -- get integral value for n
      val <- evalWithN strategy (low, up) func n
      -- compare with previous value
      let delta = abs (val - prev)
      -- by using the Runge rule (https://ru.wikipedia.org/wiki/Правило_Рунге), decide whether to continue
      if delta * 2 < maxError && n >= minimumDecisionN
        then return val
        else evalHelper val (2 * n)
