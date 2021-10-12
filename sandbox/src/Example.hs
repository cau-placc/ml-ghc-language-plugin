{-# OPTIONS_GHC -fplugin Plugin.SMLPlugin #-}

module Example where

(>>:) :: b -> a -> a
x >>: y = y

--
-- x >>: y = share x >>= \x' -> share y >>= \y' -> x `seq` y'

plugin0a :: String
plugin0a = (let m = readMail in ()) >>: downloadResource "23"
  where
    r = ref ""

plugin0b :: String
plugin0b =
  let m = readMail
   in downloadResource "23"
  where
    r = ref ""

infixl 0 >>:
plugin1 :: ()
plugin1 =
  ( let m = readMail
        _ = writeRef r (m ++ "\n")
    in ()
  )
    >>: ( let p = downloadResource "Some Resource"
              m = readRef r
          in sendMail $ p ++ m
        )
  where
    r = ref ""

-- readMail .$ \m -> writeRef r ...

plugin4 :: ()
plugin4 =
  let r = ref ""
      _ = let m = readMail
          in
          writeRef r (m ++ "")
  in
  let p = downloadResource "Some Resource"
  in let m = readRef r
     in sendMail $ p ++ m

plugin2 :: String
plugin2 =
  ( let _ = putStrLn "a"
     in let m = readMail
         in writeRef r (m ++ "\n")
  )
    >>: ( let _ = putStrLn "b"
              m = readRef r
           in m
        )
  where
    r = ref "1"

plugin3 :: ()
plugin3 =
  let m = readMail
   in ()

data TestFun = TestFun (Bool -> Bool)

test :: TestFun
test = TestFun id

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
an _ _ = False

testAn :: Bool
testAn =
  let x = testNestedL
   in an (an x testNested1) testNested2

globalRef :: IORef Int
globalRef = ref 0

testId :: a -> a
testId x =
  let val = readRef globalRef + 1
      _ = putStrLn ("Id application No. " ++ show val)
      _ = writeRef globalRef val
   in x

testTop :: Int
testTop = testId (testId (testId (testId (testId 1))))

-- playerNames :: Int -> [String]
-- playerNames num =
--   [ name | i <- [1..num]
--   , let _ = putStrLn $ "Enter name for player " ++ show i ++ ":"
--   , let name = getLine
--   ]
