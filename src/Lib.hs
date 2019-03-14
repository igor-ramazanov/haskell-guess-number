{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib
  ( program
  , Console
  , RandomGenerator
  , send
  , receive
  , generate
  ) where

import           Control.Monad
import           Control.Monad.State.Lazy
import           Debug.Trace
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
  let yes = translate lang Yes
  let no = translate lang No
  case input of
    x
      | x == yes -> return True
    x
      | x == no -> return False
    _ -> checkContinue lang
