{-# LANGUAGE OverloadedStrings #-}

module Main where

import Foreign.Ruby
import Foreign.Ruby.Bindings
-- import Foregin.C.String

printRubyIntOne :: RubyInterpreter -> IO ()
printRubyIntOne ri = do
    putStrLn "====== Int ======"
    -- Haskell => Ruby
    Right rubyOne <- toRuby ri (1 :: Int)
    putStr "Haskell => Ruby: "
    print rubyOne

    -- Ruby => Haskell
    Right haskellOne <- fromRuby ri rubyOne :: IO (Either RubyError Int) -- NOTE: type notation is necessary
    putStr "Ruby => Haskell:"
    print haskellOne

printRubyIntArray :: RubyInterpreter -> IO ()
printRubyIntArray ri = do
    putStrLn "====== Array Int ======"    
    -- Haskell => Ruby
    Right rubyArr <- toRuby ri ([5, -3, 93, -73, 2] :: [Int])
    putStr "Haskell => Ruby: "
    print rubyArr

    -- Ruby => Haskell
    Right haskellList <- fromRuby ri rubyArr :: IO (Either RubyError [Int]) -- NOTE: type notation is necessary
    putStr "Ruby => Haskell:"
    print haskellList


-- Array#length
doMethodCall1 :: RubyInterpreter -> IO ()
doMethodCall1 ri = do
    putStrLn "====== Array#length ======"
    -- Haskell => Ruby
    Right rubyArr <- toRuby ri ([5, -3, 93, -73, 2] :: [Int])
    putStr "Haskell => Ruby: "
    print rubyArr

    -- call Array#length
    lengthRID <- rb_intern "length"
    rubyLength  <- rb_funcall rubyArr lengthRID []

    putStr "rubyArr.length: "
    Right haskellLength <- fromRuby ri rubyLength :: IO (Either RubyError Int)
    print haskellLength

-- Array#*
doMethodCall2 :: RubyInterpreter -> IO ()
doMethodCall2 ri = do
    putStrLn "====== Array#* ======"
    -- Haskell => Ruby
    Right rubyArr   <- toRuby ri ([5, -3, 93, -73, 2] :: [Int])
    Right rubyThree <- toRuby ri (3 :: Int)

    -- call Array#*
    timesRID <- rb_intern "*"
    rubyTimesed  <- rb_funcall rubyArr timesRID [rubyThree]

    putStr "rubyArr * 3: "
    Right haskellTimesedArr <- fromRuby ri rubyTimesed :: IO (Either RubyError [Int])
    print haskellTimesedArr

-- 
mySafeMethodCall :: RubyInterpreter
    -> RValue
    -> String
    -> [RValue]
    ->  IO (Either RubyError RValue)
mySafeMethodCall ri reciever methodName args = makeSafe ri $ do
    rid <- rb_intern methodName
    rb_funcall reciever rid args

-- Array#*
doMethodCall3 :: RubyInterpreter -> IO ()
doMethodCall3 ri = do
    putStrLn "====== Array#* ======"
    -- Haskell => Ruby
    Right rubyArr   <- toRuby ri ([5, -3, 93, -73, 2] :: [Int])
    Right rubyThree <- toRuby ri (3 :: Int)

    -- call Array#*
    Right rubyTimesed  <- mySafeMethodCall ri rubyArr "*" [rubyThree]

    putStr "rubyArr * 3: "
    Right haskellTimesedArr <- fromRuby ri rubyTimesed :: IO (Either RubyError [Int])
    print haskellTimesedArr
    
main :: IO ()
main = do
    withRubyInterpreter $ \ri -> do
        printRubyIntOne ri
        printRubyIntArray ri
        doMethodCall1 ri
        doMethodCall2 ri
        doMethodCall3 ri        