module Main (main) where

import Integral
import Options.Applicative
import Options.Applicative.Types
import qualified Language.Haskell.Interpreter as Hint
import Data.Text (toLower)
import qualified Data.Text as T

main :: IO ()
main = do
  runAction =<< execParser opts
  where
    -- Задает парсер аргументов actionParser, сопровождая его автоматической генерацией странички help.
    opts =
      info
        (actionParser <**> helper)
        ( fullDesc
            <> progDesc "This application helps to calculate definite integrals of one variable over the real line. Improper integrals, infinite boundaries, diverging integrals are supported."
            <> header "Simple integral calculator in Haskell"
        )

-- Тип входных данных
data Input
  = StrInput String -- Строка, передаваемая аргументом командной строки
  deriving (Show)

data BoundsInput = StrBoundsInput Bounds deriving (Show)
data MaxErrorInput = StrMaxErrorInput Double deriving (Show)
data StrategyInput = StrStrategyInput (Either () Strategy) deriving (Show)

actionParser :: Parser Action
actionParser = Action <$> boundsParser <*> inputParser <*> maxErrorParser <*> strategyParser

-- Парсер аргумента, специфицирующий, откуда брать входные данные
inputParser :: Parser Input
inputParser = StrInput <$> strArgument (metavar "STRING" <> help "Integral formula")


doubleArgument :: Mod ArgumentFields Double -> Parser Double
doubleArgument = argument $ read <$> readerAsk

readBound :: String -> Bound Double
readBound str
  | str == "PlusInfinity" || str == "+\\infty" || str == "+∞" =
    PlusInfinity
  | str == "MinusInfinity" || str == "-\\infty" || str == "-∞" =
    MinusInfinity
  | otherwise = Val $ read str
boundArgument :: Mod ArgumentFields (Bound Double) -> Parser (Bound Double)
boundArgument = argument $ readBound <$> readerAsk

boundsParser :: Parser BoundsInput
boundsParser = StrBoundsInput <$> (Bounds <$> boundArgument (metavar "UPPER_BOUND" <> help "Upper bound of the integral") <*> boundArgument (metavar "LOWER_BOUND" <> help "Lower bound of the integral"))
maxErrorParser :: Parser MaxErrorInput
maxErrorParser = StrMaxErrorInput <$> doubleArgument (metavar "MAX_ERROR" <> help "Maximum absolute error for the calculation")

readStrategy :: String -> (Either () Strategy)
readStrategy str
  | toLower (T.pack str) == T.pack "rectangle" = Right Rectangle
  | toLower (T.pack str) == T.pack "trapezoid" = Right Trapezoid
  | toLower (T.pack str) == T.pack "paraboloid" = Right Paraboloid
  | toLower (T.pack str) == T.pack "all" = Left () -- all
  | otherwise = error $ "unknown strategy: " ++ str

strategyParser :: Parser StrategyInput
strategyParser = StrStrategyInput <$> (option $ readStrategy <$> readerAsk) (long "strategy" <> short 's' <> metavar "STRATEGY" <> help "Utilized calculation strategy" <> Options.Applicative.value (Left ()))

-- Тип данных, агрегирующий все аргументы командной строки, возвращается actionParser-ом
data Action = Action {
    boundsInput :: BoundsInput,
    input :: Input,
    maxError :: MaxErrorInput,
    strategy :: StrategyInput
} deriving (Show)

parseFunc :: String -> IO (Either Hint.InterpreterError Func)
parseFunc s = Hint.runInterpreter $ do
  Hint.setImports ["Prelude"]
  Hint.interpret s (Hint.as :: Func)

report :: (Either IntegralError IntegralResult) -> IO ()
report res = do
  case res of
    Left ie -> print ie
    Right ir -> print ir


-- Основная функция приложения
runAction :: Action -> IO ()
runAction (Action (StrBoundsInput bounds) (StrInput input) (StrMaxErrorInput maxError) (StrStrategyInput strategy_)) = do
  func <- parseFunc input
  case func of
    Left ie -> print ie
    Right f -> case strategy_ of
      Left _ -> do
          testStrategy Rectangle
          testStrategy Trapezoid
          testStrategy Paraboloid
      Right strategy -> do
        testStrategy strategy  
      where 
        testStrategy strategy = do
          putStrLn $ "Strategy: " ++ show strategy
          report $ eval (IntegralProps strategy maxError) bounds f
          putStrLn "==="
