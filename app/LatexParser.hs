module LatexParser where

import qualified Data.Text as T
import GHC.Float (expts)
import Integral
import Text.TeXMath.Readers.TeX
import Text.TeXMath.Types

data ParserType = Latex deriving (Show, Eq)

parseParamExp :: Char -> [Exp] -> Either T.Text Func
parseParamExp _ _ = undefined -- TODO

parseBound :: Exp -> Either T.Text (Bound Double)
parseBound e
  | e == EGrouped [ESymbol Op (T.pack "+"), ESymbol Ord (T.pack "\8734")] =
    return PlusInfinity
  | e == EGrouped [ESymbol Op (T.pack "\8722"), ESymbol Ord (T.pack "\8734")] =
    return MinusInfinity
  | otherwise = do
    f <- parseParamExp '?' [e]
    Right $ Val $ f 0

parseBounds :: Exp -> Either T.Text Bounds
-- \int \limits_{...}^{...}
parseBounds (EUnderover _ (ESymbol Op intChar) exp1 exp2)
  | T.unpack intChar == "∫" = do
    b1 <- parseBound exp1
    b2 <- parseBound exp2
    return $ Bounds b1 b2
  | otherwise = Left $ T.pack "invalid integral sign!"
-- \int_{...}^{...}
parseBounds (ESubsup (ESymbol Op intChar) exp1 exp2)
  | T.unpack intChar == "∫" = do
    b1 <- parseBound exp1
    b2 <- parseBound exp2
    return $ Bounds b1 b2
  | otherwise = Left $ T.pack "invalid integral sign!"
parseBounds _ = Left $ T.pack "invalid integral object!"

parseDX :: Exp -> Either T.Text Char
parseDX (EIdentifier e) = Right $ head $ T.unpack e
parseDX _ = Left $ T.pack "last object is not an identifier!"

parseIntegral :: [Exp] -> Either T.Text (Bounds, Func)
parseIntegral exps = do
  -- limits
  let intObj = head exps
  bounds <- parseBounds intObj
  -- dx
  let xObj = last exps
  dx <- parseDX xObj
  -- func
  let funcExps = drop 1 $ take (length exps - 2) exps
  f <- parseParamExp dx funcExps
  return (bounds, f)

parseEval :: ParserType -> T.Text -> IntegralProps -> Either T.Text (Either IntegralError IntegralResult)
parseEval Latex input props = do
    ast <- readTeX input
    (bounds, f) <- parseIntegral ast
    return $ eval props bounds f