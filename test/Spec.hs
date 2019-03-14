{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

import           Control.Exception.Base   (assert, evaluate)
import           Control.Monad            (when)
import           Control.Monad.State.Lazy (State, execState, state)
import           Lib                      (program, Console, RandomGenerator, send, receive, generate)
import           Output

initialTestData = testData ["Igor", "1", "n"] [] [1]

expectedOutput = [AskName, Greetings "Igor", GuessNumber, Correct, WantContinue]

main :: IO ()
main = do
  let res = reverse $ output $ run initialTestData
  Control.Monad.when (res /= expectedOutput) $ error "Boom"

run :: TestData -> TestData
run = execState (program English)

data TestData = TestData
  { input   :: [String]
  , output  :: [Output]
  , numbers :: [Int]
  } deriving (Show, Eq)

testData :: [String] -> [Output] -> [Int] -> TestData
testData i o n = TestData {input = i, output = o, numbers = n}

appendToOutput :: TestData -> Output -> ((), TestData)
appendToOutput t s =
  ((), TestData {output = s : output t, input = input t, numbers = numbers t})

getInput :: TestData -> (String, TestData)
getInput t =
  let line = head $ input t
      rest = tail $ input t
      updatedData =
        TestData {input = rest, output = output t, numbers = numbers t}
   in (line, updatedData)

getNumber :: TestData -> (Int, TestData)
getNumber t =
  let n = head $ numbers t
      rest = tail $ numbers t
      updatedData =
        TestData {numbers = rest, input = input t, output = output t}
   in (n, updatedData)

instance Console (State TestData) where
  send _ s = state $ \t -> appendToOutput t s
  receive = state getInput

instance RandomGenerator (State TestData) where
  generate = state getNumber