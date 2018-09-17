-- | Main implementation of slim server for testing purpose
import           Control.Concurrent.Async
import           Slim
import           System.Environment

main :: IO ()
main = do
  [portString] <- getArgs
  thread <- startSlimServer (read portString)
  wait thread
