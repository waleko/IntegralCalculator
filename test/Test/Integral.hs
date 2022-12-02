module Test.Integral where

import Integral
import Test.HUnit (Assertion, assertBool, (@?=))
import Test.HUnit.Approx (assertApproxEqual)

inv x = 1 / x
x2 x = x * x
x3 x = x * x * x

eps = 0.01

unit_simple :: Assertion
unit_simple = do
    let f = ((*3) . x2)
    -- :((
    let r = eval (IntegralProps Rectangle 0.01) (Bounds (Val 0) (Val 2)) f
    case r of 
        Left ie -> False @?= True -- :((
        Right (IntegralResult val _) -> assertApproxEqual "3x^3 rectangle failed" 0.01 val 8
