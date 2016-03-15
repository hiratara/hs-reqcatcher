{-# LANGUAGE OverloadedStrings #-}
import qualified Control.Exception as EX
import Control.Lens ((.~), (&), (^.))
import qualified Data.List as LS
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai as WAI
import qualified Network.Wreq as WR
import Test.HUnit (assertFailure)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps, (@?), (@=?))

import qualified Web.ReqCatcher as RC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [testReqCatcher]

testReqCatcher :: TestTree
testReqCatcher = testCaseSteps "General usage of ReqCatcher" $ \step -> do
  step "new instance"
  catcher <- RC.newCatcher

  step "check URL"
  let url = RC.catcherUrl catcher
  "http" `LS.isPrefixOf` url @? "Invalid URL: " ++ url

  step "Throw an http request to URL"
  let opts = WR.defaults & WR.param "foo" .~ ["bar"]
  r <- WR.getWith opts url
  (r ^. WR.responseStatus) @=? HTTP.ok200

  step "Catch the request"
  r' <- RC.catchRedirect catcher
  let Just (Just bar) = lookup "foo" (WAI.queryString r')
  bar @=? "bar"

  step "Shutdown the server"
  EX.handle
    (\e -> let e' = e :: HTTP.HttpException in return ()) $
    do r <- WR.getWith opts url
       assertFailure ("The http server is alive: " ++ show r)

  return ()
