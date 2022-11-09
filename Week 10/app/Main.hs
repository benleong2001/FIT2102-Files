import           Control.Monad
import           FileIO
import           Prelude

import qualified System.Environment            as E

-- | Read the arguments passed to `main`.
getArgs :: IO [String]
getArgs = E.getArgs

-- | Prompt user for a file containing a list of files and print their content,
-- and report the total number of characters in the files.
--
-- You can run this function in GHCi by calling:
-- > :main "input.txt"
main :: IO ()
main = do
  args  <- getArgs
  count <- if length args == 0
    then do
      putStrLn "Enter filename:"
      fp <- getLine
      run fp
    else run (head args)
  putStrLn $ "Characters = " ++ show count
