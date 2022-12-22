module Main (main) where

import Data.Text (toLower)
import qualified Data.Text as T
import Integral
import qualified Language.Haskell.Interpreter as Hint
import Options.Applicative
import Options.Applicative.Types

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
            <> header "calcigral: Simple integral calculator in Haskell"
        )

-- Тип входных данных
data Input
  = StrInput String -- Строка, передаваемая аргументом командной строки
  deriving (Show)

data BoundsInput = StrBoundsInput Bounds deriving (Show)

data MaxErrorInput = StrMaxErrorInput Double deriving (Show)

data StrategyInput = StrStrategyInput (Either () Strategy) deriving (Show)

data MaxStepsInput = StrMaxStepsInput Int deriving (Show)

actionParser :: Parser Action
actionParser = Action <$> boundsParser <*> inputParser <*> maxErrorParser <*> strategyParser <*> maxStepsParser

-- Парсер аргумента, специфицирующий, откуда брать входные данные
inputParser :: Parser Input
inputParser = StrInput <$> strArgument (metavar "FORMULA" <> help "Integral function (given as a haskell function). Example: sin, cos, \\x -> x * x * 3, \\t -> t * sin t")

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
boundsParser = StrBoundsInput <$> (Bounds <$> boundArgument (metavar "LOWER_BOUND" <> help "Lower bound of the integral. Example: -21.2, 0, MinusInfinity") <*> boundArgument (metavar "UPPER_BOUND" <> help "Upper bound of the integral. Example: 1, 3.43, PlusInfinity"))

maxErrorParser :: Parser MaxErrorInput
maxErrorParser = StrMaxErrorInput <$> doubleArgument (metavar "MAX_ERROR" <> help "Maximum absolute error for the calculation (default: 1e-3)" <> Options.Applicative.value 0.001)

readStrategy :: String -> Either () Strategy
readStrategy str
  | toLower (T.pack str) == T.pack "rectangle" = Right Rectangle
  | toLower (T.pack str) == T.pack "trapezoid" = Right Trapezoid
  | toLower (T.pack str) == T.pack "paraboloid" = Right Paraboloid
  | toLower (T.pack str) == T.pack "all" = Left () -- all
  | otherwise = error $ "unknown strategy: " ++ str

strategyParser :: Parser StrategyInput
strategyParser = StrStrategyInput <$> (option $ readStrategy <$> readerAsk) (long "strategy" <> short 's' <> metavar "STRATEGY" <> help "Utilized calculation strategy (possible values: Rectangle | Trapezoid | Paraboloid | All)" <> Options.Applicative.value (Left ()))

intArgument :: Mod ArgumentFields Int -> Parser Int
intArgument = argument $ read <$> readerAsk

maxStepsParser :: Parser MaxStepsInput
maxStepsParser = StrMaxStepsInput <$> intArgument (metavar "MAX_STEPS" <> help "Maximum steps for calculation, until integral is considered diverging (default: 1e5)" <> Options.Applicative.value 10000)

-- Тип данных, агрегирующий все аргументы командной строки, возвращается actionParser-ом
data Action = Action
  { boundsInput :: BoundsInput,
    input :: Input,
    maxError :: MaxErrorInput,
    strategy :: StrategyInput,
    maxSteps :: MaxStepsInput
  }
  deriving (Show)

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
runAction (Action (StrBoundsInput bounds) (StrInput input) (StrMaxErrorInput maxError) (StrStrategyInput strategy_) (StrMaxStepsInput maxSteps)) = do
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
          report $ eval (IntegralProps strategy maxError maxSteps) bounds f
          putStrLn "==="
