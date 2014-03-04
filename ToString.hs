module ToString (toString) where

import Data.Maybe
import Text.Read

toString :: Show a => a -> String
toString a = let s = show a in fromMaybe s (readMaybe s)

main :: IO ()
main = do
  putStrLn (toString "foo")
  putStrLn (toString 23)
  putStrLn (toString "λ-scientist")
  putStrLn (toString "\"foo\"")
