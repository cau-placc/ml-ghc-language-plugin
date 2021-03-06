module Tests (tests) where

import Distribution.TestSuite
import System.Directory
import System.Process
import System.Exit
import System.Environment

tests :: IO [Test]
tests = do
  path <- makeAbsolute "test-examples"
  setCurrentDirectory  path
  args' <- getArgs
  let args = if null args' then ["Semantic", "Compile"] else args'
  return
    [ if "Compile" `notElem` args then noTest else testGroup "Compile Tests"
    [ Test (mkCompileTest ExpectFail "ImportHaskell.hs")
    , Test (mkCompileTest Succeed    "PatternMatching.hs")
    , Test (mkCompileTest Succeed    "Record.hs")
    , Test (mkCompileTest Succeed    "InstanceImport.hs")
    , Test (mkCompileTest Succeed    "Typeclass.hs")
    , Test (mkCompileTest Succeed    "OverloadedStrings.hs")
    , Test (mkCompileTest Succeed    "MultiParamFlexible.hs")
    , Test (mkCompileTest Succeed    "ScopedTypeVariables.hs")
    , Test (mkCompileTest Succeed    "GeneralizedNewtypeDeriving.hs")
    , Test (mkCompileTest Succeed    "TypeOperatorsImport.hs")
    , Test (mkCompileTest Succeed    "TupleSections.hs")
    , Test (mkCompileTest Succeed    "KindSignatures.hs")
    , Test (mkCompileTest Succeed    "DefaultSignaturesImport.hs")
    ]
    , if "Semantic" `notElem` args then noTest else testGroup "Semantic Tests"
    [
    ]]
  where noTest = testGroup "Empty Group" []

data Expected = Succeed | ExpectFail

mkCompileTest :: Expected -> FilePath -> TestInstance
mkCompileTest expect file = TestInstance
  { run = testGhcInvocation expect file
  , name = file
  , tags = ["Compile"]
  , options = []
  , setOption = \_ _ -> Left "Option not supported"
  }

testGhcInvocation :: Expected -> FilePath -> IO Progress
testGhcInvocation expect file = do
  process <- spawnProcess "ghc"
    ["-hidir out", "-odir out", "-fforce-recomp", "-dcore-lint", file]
  code <- waitForProcess process
  return $ case code of
    ExitSuccess   | Succeed    <- expect
      -> Finished Pass
    ExitSuccess   | ExpectFail <- expect
      -> Finished (Fail "Compilation succeeded, but was expected to fail")
    ExitFailure _ | ExpectFail <- expect
      -> Finished Pass
    ExitFailure _ | Succeed    <- expect
      -> Finished (Fail "Compilation failed unexpectedly")
