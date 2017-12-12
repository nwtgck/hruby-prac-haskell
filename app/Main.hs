module Main where

import Foreign.Ruby

main :: IO ()
main = do
    ri            <- startRubyInterpreter
    Right rubyOne <- toRuby ri (1 :: Int)
    print rubyOne
    closeRubyInterpreter ri