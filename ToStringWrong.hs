
toString :: Show a => a -> String
toString = trimQuotes . show

trimQuotes :: String -> String
trimQuotes = reverse . dropWhile (== '"') . reverse . dropWhile (== '"')

main :: IO ()
main = do
  putStrLn (toString "foo")
  -- putStrLn (toString 23)
  -- putStrLn (toString "λ-scientist")
  -- putStrLn (toString "\"foo\"")
