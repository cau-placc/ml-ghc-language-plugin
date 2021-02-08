{-# OPTIONS_GHC -fplugin Plugin.SMLPlugin #-}
module Example where

data Test = Test Int


testShare :: Test
testShare = Test (let _ = print "Hello" in 5 :: Int)

globalRef :: IORef Int
globalRef = ref 0

testId :: a -> a
testId x = let val = readRef globalRef + 1
               _   = putStrLn ("Id application No. " ++ show val)
               _   = writeRef globalRef val
            in x

testTop :: Int
testTop = testId (testId (testId (testId (testId 1))))

playerNames :: Int -> [String]
playerNames num =
  [ name | i <- [1..num]
  , let _ = putStrLn $ "Enter name for player " ++ show i ++ ":"
  , let name = getLine
  ]
