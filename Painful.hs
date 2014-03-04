name :: String
name = "Marvin"

age :: Int
age = 23

profession :: String
profession = "λ-scientist"

main :: IO ()
main = do
  putStrLn (name ++ " is a " ++ show age ++ "-year-old " ++ profession ++ "!")
