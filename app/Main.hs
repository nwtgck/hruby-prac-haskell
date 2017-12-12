module Main where

import Foreign.Ruby

printRubyIntOne :: IO ()
printRubyIntOne = 
    withRubyInterpreter $ \ri -> do
        -- Haskell => Ruby
        Right rubyOne <- toRuby ri (1 :: Int)
        putStr "Haskell => Ruby `1`: "
        print rubyOne

        -- Ruby => Haskell
        Right haskellOne <- fromRuby ri rubyOne :: IO (Either RubyError Int)
        putStr "Ruby => Haskell `1`:"
        print haskellOne

main :: IO ()
main = do
    printRubyIntOne