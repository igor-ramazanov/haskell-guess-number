import           Control.Exception.Base   (assert, evaluate)
import           Control.Monad            (when)
import           Control.Monad.State.Lazy (State, execState)
import           Lib                      (TestData, output, program, testData)
import           Output

initialTestData = testData [] [] []

expectedOutput = [AskName, Greetings "Igor", GuessNumber, Correct, WantContinue]

main :: IO ()
main = do
  out <- run initialTestData
  let res = reverse $ output out
  print res
  Control.Monad.when (res /= expectedOutput) $ error "Boom"

run :: TestData -> IO TestData
run t = do
  print t
  return (execState (program Russian) t)
