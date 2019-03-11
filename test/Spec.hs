import           Control.Exception.Base   (assert, evaluate)
import           Control.Monad            (when)
import           Control.Monad.State.Lazy (State, execState)
import           Lib                      (TestData, output, program, testData)
import           Output

initialTestData = testData ["Igor", "1", "n"] [] [1]

expectedOutput = [AskName, Greetings "Igor", GuessNumber, Correct, WantContinue]

main :: IO ()
main = do
  let res = reverse $ output $ run initialTestData
  Control.Monad.when (res /= expectedOutput) $ error "Boom"

run :: TestData -> TestData
run = execState (program English)
