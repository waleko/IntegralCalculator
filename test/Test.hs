import qualified Test.Integral
import qualified Test.Steps
import Test.Tasty

main :: IO ()
main = do
  defaultMain
    ( testGroup
        "All Tests"
        [ testGroup "Unit" Test.Integral.unitTests,
          testGroup "Steps Monad" Test.Steps.props
        ]
    )