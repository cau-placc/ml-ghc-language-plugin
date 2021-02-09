{-# OPTIONS_GHC -fplugin Plugin.SMLPlugin #-}
module Example where

data Test = Test Int

testShare :: Test
testShare = Test (let _ = print "Hello" in 5 :: Int)

testNested1 :: Bool
testNested1 = case testShare of
  (Test _) -> True

testNested2 :: Bool
testNested2 = case testShare of
  (Test _) -> True

testNestedL :: Bool
testNestedL = case testShare of
  (Test ~x) -> True

an :: Bool -> Bool -> Bool
an True True = True
an _ _       = False

testAn :: Bool
testAn = let x = testNestedL
  in an (an x testNested1) testNested2

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
