{-# LANGUAGE QuasiQuotes #-}
import Text.InterpolatedString.QQ

name :: String
name = "Marvin"

age :: Int
age = 23

profession :: String
profession = "λ-scientist"

main :: IO ()
main = do
  putStrLn [istr|name: #{name}|]
  -- putStrLn [istr|age: #{age}|]
  -- putStrLn [istr|profession: #{profession}|]
  -- putStrLn [istr|#{name} is a #{age}-year-old #{profession}!|]
