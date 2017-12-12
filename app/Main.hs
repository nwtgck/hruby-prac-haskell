module Main where

import Foreign.Ruby

printRubyIntOne :: IO ()
printRubyIntOne = do
    ri            <- startRubyInterpreter

    -- Haskell => Ruby
    Right rubyOne <- toRuby ri (1 :: Int)
    putStr "Haskell => Ruby `1`: "
    print rubyOne

    -- Ruby => Haskell
    Right haskellOne <- fromRuby ri rubyOne :: IO (Either RubyError Int)
    putStr "Ruby => Haskell `1`:"
    print haskellOne

    closeRubyInterpreter ri

main :: IO ()
main = do
    printRubyIntOne