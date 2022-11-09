module Main where

import           Prelude
import           System.Environment

-- | Prompt user for a JSON file, parse it and pretty print the result.
-- For example, it sould print new lines after each '[',']','{','}' or ','
-- and '[' or '{' should increase the indent level while ']' or '}' should decrease the indent level.
-- You can try it out with the JSON examples in <code>share/</code>.
--
-- /Tip:/ use @getArgs@, @readJsonFile@ and @putStrLn@
--
-- You can run this function in GHCi by calling:
-- > :main "input.json"
main :: IO ()
main = putStrLn "Not implemented"
