module Test.Integral where

import Integral
import Test.HUnit (Assertion, assertEqual, assertFailure)
import Test.HUnit.Approx (assertApproxEqual)

defaultEps = 0.001

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

checkEval :: Bounds -> Func -> Either IntegralError Double -> Double -> Assertion
checkEval bounds f expected eps = do
  checkEvalHelper "[evalRectangle]" expected (IntegralProps Rectangle eps) bounds f
  checkEvalHelper "[evalTrapezoid]" expected (IntegralProps Trapezoid eps) bounds f
  checkEvalHelper "[evalParaboloid]" expected (IntegralProps Paraboloid eps) bounds f

unit_simple :: Assertion
unit_simple = do
  checkEval (Bounds (Val 0) (Val 2)) (\x -> x * x * 3) (Right 8) defaultEps
  checkEval (Bounds (Val 0) (Val 5)) (\x -> sin (x * x) + sqrt x) (Right 7.981477206) 0.001
  -- checkEval (Bounds (Val $ -0.1) (Val 0.1)) (\x -> sin (1 / x) + cos (1 / x) / x) (Right 0) defaultEps -- function is too messy
  checkEval (Bounds MinusInfinity PlusInfinity) (\x -> exp (- x * x)) (Right $ sqrt pi) defaultEps
  checkEval (Bounds (Val 0) PlusInfinity) (\x -> exp (- x * x)) (Right $ (sqrt pi) / 2) defaultEps

unit_diverging :: Assertion
unit_diverging = do
  -- checkEval (Bounds (Val 0) (Val 1)) (\x -> 1 / (x * x)) (Left Diverging) defaultEps -- delta does not approach 0, so it doesn't detect divergence :(
  checkEval (Bounds (Val 0) (Val 1)) (\x -> 1 / (x * x)) (Left Diverging) defaultEps

unit_invalid_bounds :: Assertion
unit_invalid_bounds = do
  checkEval (Bounds MinusInfinity MinusInfinity) id (Left InvalidBounds) defaultEps
