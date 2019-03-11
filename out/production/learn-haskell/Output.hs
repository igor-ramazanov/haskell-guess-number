module Output where

data Output
  = AskName
  | Greetings String
  | GuessNumber
  | Failed String
  | Correct
  | Wrong String
  | WantContinue
  | Yes
  | No
  deriving (Show, Eq)

data Language = Russian | English

translate ::Language -> Output -> String
translate English o = en o
translate Russian o = ru o

en :: Output -> String
en AskName = "What's your name?"
en (Greetings s) = "Hello, " ++ s ++ "!"
en GuessNumber = "Guess a number!"
en (Failed s) = "Failed to parse '" ++ s ++ "' as integer"
en Correct = "Correct!"
en (Wrong s) = "Wrong answer, correct number is " ++ s
en WantContinue = "Want to continue? ('y' or 'n')"
en Yes = "y"
en No =  "n"

ru :: Output -> String
ru AskName = "Как тебя зовут?"
ru (Greetings s) = "Привет, " ++ s ++ "!"
ru GuessNumber = "Попробуй отгадать число, которое я загадал!"
ru (Failed s) = "Не смог прочитать'" ++ s ++ "' как целое число"
ru Correct = "Верно!"
ru (Wrong s) = "Неправильно, верный ответ - " ++ s
ru WantContinue = "Хотите продолжить? ('да' или 'нет')"
ru Yes = "да"
ru No = "нет"