module Test.Steps where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Range as Range
import Integral
import Test.Tasty
import Test.Tasty.Hedgehog

-- | Binary tree of unit operations. Each leaf returns a number.
data OpTree = OpLeaf (() -> Int) | OpNode OpTree OpTree

instance Show OpTree where
  show (OpLeaf _) = "OpLeaf"
  show (OpNode l r) = "OpNode(l=" ++ show l ++ ", r=" ++ show r ++ ")"

-- | Generator for op trees
genTree :: MonadGen m => m OpTree
genTree =
  Gen.recursive
    Gen.choice
    [pure $ OpLeaf $ const 1]
    [Gen.subterm2 genTree genTree OpNode]

-- | Counts the number of leaves in a tree
countLeaves :: OpTree -> Int
countLeaves (OpLeaf f) = f ()
countLeaves (OpNode a b) = countLeaves a + countLeaves b

-- | Unwraps the tree into a monad
convertToMonad :: OpTree -> ResultWithSteps Int
convertToMonad (OpLeaf f) = unit $ f ()
convertToMonad (OpNode a b) = do
  va <- convertToMonad a
  vb <- convertToMonad b
  return $ va + vb

-- | Infinite recursion function
infFunc :: () -> Int
infFunc = infFunc

-- | Tests `ResultWithSteps` monad that counts the number of steps, and halts execution if the number exceeds the pre-set threshold.
prop_steps :: Property
prop_steps = property $ do
  -- generates the op tree
  tree <- forAll genTree
  let n = countLeaves tree
  let result = convertToMonad tree

  -- check if not enough steps, we get cancelled
  notEnough <- forAll $ Gen.integral (Range.constantFrom n 0 (n - 1))
  runWithLimit result notEnough === CancelledValue

  -- check correct steps left, if enough
  offset <- forAll $ Gen.integral (Range.constantFrom 50 1 1000)
  runWithLimit result (n + offset) === ValueWithSteps (n, offset)

  -- check that no further functions are executed after being cancelled
  let strictNTree = OpNode tree (OpLeaf infFunc)
  runWithLimit (convertToMonad strictNTree) n === CancelledValue

props :: [TestTree]
props = [testProperty "Test result with steps" prop_steps]
