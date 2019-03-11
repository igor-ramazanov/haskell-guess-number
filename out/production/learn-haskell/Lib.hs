{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib
  ( program
  , TestData
  , input
  , output
  , numbers
  , testData
  ) where

import           Control.Monad
import           Control.Monad.State.Lazy
import           Output
import           System.Random
import           Text.Read

class Console m where
  send :: Language -> Output -> m ()
  receive :: m String

instance Console IO where
  send lang o = putStrLn $ translate lang o
  receive = getLine

class RandomGenerator m where
  generate :: m Int

instance RandomGenerator IO where
  generate = fmap ((`mod` 5) . (+ 1)) randomIO

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

program :: (Monad m, Console m, RandomGenerator m) => Language -> m ()
program lang = do
  send lang AskName
  name <- receive
  send lang $ Greetings name
  n <- generate
  send lang GuessNumber
  input <- receive
  let guess = readMaybe input :: Maybe Int
  maybe
    (send lang $ Failed input)
    (\x ->
       if x == n
         then send lang Correct
         else send lang $ Wrong $ show n)
    guess
  continue <- checkContinue lang
  Control.Monad.when continue $ program lang

checkContinue :: (Monad m, Console m) => Language -> m Bool
checkContinue lang = do
  send lang WantContinue
  input <- receive
  if | input == ru Yes -> return True
     | input == ru No -> return False
     | otherwise -> checkContinue lang
