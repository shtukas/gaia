module Lib (
  someFunc,
  someOtherFunc
) where

import           Directives

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- horrible, it's here just for fun :)
someOtherFunc :: IO ()
someOtherFunc = do
  putStrLn "Strict directive parser over correct input"
  value <- return $ parseDirectives "gaia\n#here is a comment\ntag: Use the force, Luke\n\n#here is a comment\n#here is a comment\ntag: only tags for the moment, with UTF-8 content: üè£∞\n"
  putStrLn $ "Parse result: " ++ show value
  putStrLn ""

  putStrLn "Strict directive parser over non-correct input"
  value <- return $ parseDirectives "gaia\n#here is a comment\ntag: Use the force, Luke\n\n#here is a comment\ngotcha: this is not a known tag\n#here is a comment\ntag: only tags for the moment, with UTF-8 content: üè£∞\n"
  putStrLn $ "Parse result: " ++ show value
  putStrLn ""

  putStrLn "End of test"
