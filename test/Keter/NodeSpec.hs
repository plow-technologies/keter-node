{-# LANGUAGE OverloadedStrings #-}
module Keter.NodeSpec (main, spec) where


import Keter.Node
import Keter.Node.Types
import Keter.App
import Keter.AppManager
import Keter.Types
import Control.Concurrent
import Test.Hspec
import Filesystem.Path.CurrentOS (directory, encodeString, (<.>),(</>),empty)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "someFunction" $ do
    it "should work fine" $ do
      True `shouldBe` False




testAppManager = do 
  asc <- simpleManager 
  let aid = AINamed "toyproc"
      ain = AIData sampleBundleConfig
  initialize (\_ -> print "log")  asc   

testFP = "" <.> ""</> "toyproc" <.> "keter"

testManagerUse = do 
  apmgr <- testAppManager 
  addApp apmgr testFP
  addApp apmgr testFP
  print "App Created!" 
  threadDelay 10000000 >> terminateApp apmgr "toyproc" >> print "app terminated"  

