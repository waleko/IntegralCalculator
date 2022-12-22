module Test.Integral where

import Integral
import Test.HUnit.Approx (assertApproxEqual)
import Test.Tasty
import Test.Tasty.HUnit

defaultEps = 0.001

defaultMaxSteps = 10000

checkEvalHelper :: String -> Either IntegralError Double -> IntegralProps -> Bounds -> Func -> Assertion
checkEvalHelper testPrefix expected props bounds f = do
  let ans = eval props bounds f
  let eps = maxError props
  case ans of
    Left ie -> case expected of
      Left expected_ie -> assertEqual (testPrefix ++ ": different errors") expected_ie ie
      Right _ -> assertFailure $ testPrefix ++ ": expected no error, but got " ++ show ie
    Right (IntegralResult res _) -> case expected of
      Left _ -> assertFailure $ testPrefix ++ ": expected error, but got result " ++ show res
      Right expected_res -> assertApproxEqual (testPrefix ++ ": eps failed") eps expected_res res

checkEval :: Bounds -> Func -> Either IntegralError Double -> Double -> Int -> Assertion
checkEval bounds f expected eps maxSteps = do
  checkEvalHelper "[evalRectangle]" expected (IntegralProps Rectangle eps maxSteps) bounds f
  checkEvalHelper "[evalTrapezoid]" expected (IntegralProps Trapezoid eps maxSteps) bounds f
  checkEvalHelper "[evalParaboloid]" expected (IntegralProps Paraboloid eps maxSteps) bounds f

unit_simple :: Assertion
unit_simple = do
  checkEval (Bounds (Val 0) (Val 2)) (\x -> x * x * 3) (Right 8) defaultEps defaultMaxSteps
  checkEval (Bounds (Val 0) (Val 5)) (\x -> sin (x * x) + sqrt x) (Right 7.981477206) 0.001 defaultMaxSteps
  -- checkEval (Bounds (Val $ -0.1) (Val 0.1)) (\x -> sin (1 / x) + cos (1 / x) / x) (Right 0) defaultEps -- function is too messy
  checkEval (Bounds MinusInfinity PlusInfinity) (\x -> exp (- x * x)) (Right $ sqrt pi) defaultEps defaultMaxSteps
  checkEval (Bounds (Val 0) PlusInfinity) (\x -> exp (- x * x)) (Right $ (sqrt pi) / 2) defaultEps defaultMaxSteps

unit_diverging :: Assertion
unit_diverging = do
  checkEval (Bounds (Val 0) PlusInfinity) (1 /) (Left Diverging) defaultEps defaultMaxSteps
  checkEval (Bounds (Val 0) (Val 1)) (\x -> 1 / (x * x)) (Left Diverging) defaultEps defaultMaxSteps

unit_invalid_bounds :: Assertion
unit_invalid_bounds = do
  checkEval (Bounds MinusInfinity MinusInfinity) id (Left InvalidBounds) defaultEps defaultMaxSteps

unitTests :: [TestTree]
unitTests =
  [ testCase "Simple integral calculation" unit_simple,
    testCase "Diverging integral" unit_diverging,
    testCase "Invalid bounds" unit_invalid_bounds
  ]