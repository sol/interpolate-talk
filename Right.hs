{-# LANGUAGE QuasiQuotes #-}
import Data.String.Interpolate

name :: String
name = "Marvin"

age :: Int
age = 23

profession :: String
profession = "λ-scientist"

main :: IO ()
main = do
  putStrLn [i|#{name} is a #{age}-year-old #{profession}!|]
