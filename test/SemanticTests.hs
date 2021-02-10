{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module SemanticTests where

import Distribution.TestSuite
import Control.Concurrent

data SemanticTestDescr = forall a. (Eq a, Show a) => TestDescr
  {  testExpr   :: IO a
  ,  testResult :: IO a
  ,  testName   :: String
  }

mkSemanticTest :: SemanticTestDescr -> TestInstance
mkSemanticTest (TestDescr e expected nm) = TestInstance
  { run = do
      res <- timeout 2000000 (e `seq` e)
      expec <- expected
      return $ case res of
        Just r | r == expec
                 -> Finished Pass
               | otherwise
                 -> Finished (Fail ("Expected: " ++ show expec ++
                                    ", but the result is: " ++ show r))
        Nothing  -> Finished (Fail "Computation timed out")
  , name = nm
  , tags = ["Semantic"]
  , options = []
  , setOption = \_ _ -> Left "Option not supported"
  }

timeout :: Int -> IO a -> IO (Maybe a)
timeout usec action = compete
  [fmap Just action, threadDelay usec >> return Nothing]

compete :: [IO a] -> IO a
compete actions = do
  mvar <- newEmptyMVar
  tids <- mapM (\action -> forkIO $ action >>= putMVar mvar) actions
  result <- takeMVar mvar
  mapM_ killThread tids
  return result
