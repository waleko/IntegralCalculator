{-# LANGUAGE BangPatterns #-}

module Test.IntegralComp where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Range as Range
import Integral
import Test.Tasty
import Test.Tasty.Hedgehog

data BinaryOp = Sum | Mul | Div deriving (Show)

data UnaryOp = Negate | Sqrt | Sin | Cos deriving (Show)

data Expr a = X | Val a | UnaryOpExpr UnaryOp (Expr a) | BinaryOpExpr BinaryOp (Expr a) (Expr a) deriving (Show)

makeUnary :: MonadGen m => Expr a -> m (Expr a)
makeUnary e = do
  op <- Gen.element [Negate, Sqrt, Sin, Cos]
  return $ UnaryOpExpr op e

makeBinary :: MonadGen m => Expr a -> Expr a -> m (Expr a)
makeBinary e1 e2 = do
  op <- Gen.element [Sum, Mul, Div]
  return $ BinaryOpExpr op e1 e2

-- | Generator of expressions
genExpr :: MonadGen m => m (Expr Double)
genExpr =
  Gen.recursive
    Gen.choice
    [ pure X,
      Test.IntegralComp.Val <$> Gen.double (Range.constantFrom 10 (-100) 100)
    ]
    [ Gen.subtermM genExpr makeUnary,
      Gen.subtermM2 genExpr genExpr makeBinary
    ]

-- | converts expression to function
convertToFunc :: Expr Double -> Double -> Double
convertToFunc X x = x
convertToFunc (Test.IntegralComp.Val v) _ = v
convertToFunc (UnaryOpExpr op e) x = case op of
  Negate -> - y
  Sqrt -> sqrt y
  Sin -> sin y
  Cos -> cos y
  where
    y = convertToFunc e x
convertToFunc (BinaryOpExpr op e1 e2) x = case op of
  Sum -> a + b
  Mul -> a * b
  Div -> a / b
  where
    a = convertToFunc e1 x
    b = convertToFunc e2 x

testEval :: Double -> Int -> Bound Double -> Bound Double -> Func -> Strategy -> EvaluationResult
testEval maxError maxSteps lowerB upperB func strategy = Integral.eval (IntegralProps strategy maxError maxSteps) (Bounds lowerB upperB) func

compareEvaluationResult :: MonadTest m => Double -> EvaluationResult -> EvaluationResult -> m ()
compareEvaluationResult _ (Left err1) (Left err2) = err1 === err2
compareEvaluationResult maxError (Right (IntegralResult val1 _)) (Right (IntegralResult val2 _)) = do
  let diff = abs $ val1 - val2
  footnoteShow diff
  assert $ diff <= 2 * maxError
compareEvaluationResult _ (Left Diverging) (Right (IntegralResult _ _)) = success -- different methods may outperform each other, therefore failing by maxSteps
compareEvaluationResult _ (Right (IntegralResult _ _)) (Left Diverging) = success -- different methods may outperform each other, therefore failing by maxSteps
compareEvaluationResult _ _ _ = failure

prop_cmp :: Property
prop_cmp = property $ do
  -- generate formula
  expr <- forAll genExpr
  let func = convertToFunc expr

  lowerB <- forAll $ Gen.choice [Integral.Val <$> Gen.double (Range.constantFrom (-10) (-100) 100), pure MinusInfinity, pure PlusInfinity]
  upperB <- forAll $ Gen.choice [Integral.Val <$> Gen.double (Range.constantFrom 10 (-100) 100), pure MinusInfinity, pure PlusInfinity]
  maxError <- forAll $ Gen.double (Range.constantFrom 0.01 0 1)
  maxSteps <- forAll $ Gen.integral (Range.constantFrom 2000 100 10000)

  let test = testEval maxError maxSteps lowerB upperB func
  annotate $ "func=" ++ show expr
  annotate $ "low=" ++ show lowerB ++ ", up=" ++ show upperB ++ ", maxError=" ++ show maxError ++ ", maxSteps=" ++ show maxSteps

  let !ans_rect = test Rectangle
  annotateShow ans_rect
  let !ans_trap = test Trapezoid
  annotateShow ans_trap
  let !ans_para = test Paraboloid
  annotateShow ans_para

  let cmp = compareEvaluationResult maxError
  cmp ans_rect ans_trap
  cmp ans_trap ans_para
  cmp ans_rect ans_para

props :: [TestTree]
props = [testProperty "Compare integral evaluation strategies" prop_cmp]
