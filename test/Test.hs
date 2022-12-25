import qualified Test.Integral
import qualified Test.Steps
import qualified Test.IntegralComp
import Test.Tasty

main :: IO ()
main = do
  defaultMain
    ( testGroup
        "All Tests"
        [ testGroup "Unit" Test.Integral.unitTests,
          testGroup "Steps Monad" Test.Steps.props,
          testGroup "Integral comparison" Test.IntegralComp.props
        ]
    )