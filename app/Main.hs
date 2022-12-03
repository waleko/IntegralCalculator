module Main (main) where

import Integral
import Options.Applicative
import Text.Printf (printf)

main :: IO ()
main = do
  runAction =<< execParser opts
  where
    -- Задает парсер аргументов actionParser, сопровождая его автоматической генерацией странички help.
    opts =
      info
        (actionParser <**> helper)
        ( fullDesc
            <> progDesc "This application demonstrates two parsers for arithmetic expressions: one for the infix and the other for the prefix notation."
            <> header "Simple parser console application"
        )

-- Тип входных данных
data Input
  = StrInput String | FileInput String -- Строка, передаваемая аргументом командной строки
  deriving (Show)

actionParser :: Parser Action
actionParser = Action <$> inputParser

-- Парсер аргумента, специфицирующий, откуда брать входные данные
inputParser :: Parser Input
inputParser = fileInput <|> strInput

-- Флаг -i/--input позволяет задать строку -- имя входного файла
fileInput :: Parser Input
fileInput =
  FileInput
    <$> strOption --
      ( short 'i' -- короткое имя флага (-i)
          <> long "input" -- длинное имя флага (--input)
          <> metavar "INPUT" -- как аргумент этой опции называется в документации
          <> help "Input file" --
      )

-- Можно не использовать флаг i, а просто написать входную строку (1+2 в stack run exe -- 1+2)
strInput :: Parser Input
strInput = StrInput <$> strArgument (metavar "STRING" <> help "String to be parsed")

-- Тип данных, агрегирующий все аргументы командной строки, возвращается actionParser-ом
data Action = Action {
    input :: Input
} deriving (Show)

-- Основная функция приложения
runAction :: Action -> IO ()
runAction (Action inp) = do
  putStrLn $ printf "aboba"
