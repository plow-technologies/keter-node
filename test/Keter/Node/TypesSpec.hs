{-# LANGUAGE OverloadedStrings #-}
module Keter.Node.TypesSpec (main, spec) where


import Keter.Node
import Keter.Node.Types
import Keter.App
import Keter.AppManager
import Keter.Types
import Control.Concurrent
import Test.Hspec
import Data.Aeson
import Filesystem.Path.CurrentOS (directory, encodeString, (<.>),(</>),empty)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "toJSON KeterNode" $ do
    it "should convert to a JSON string" $ do                                  
      (encode testKeterNode) `shouldBe` "{\"KeterNode\":{\"Right\":\"./\"}}"
  describe "fromJSON KeterNode" $ do
    it "should convert to a KeterNode" $ do                                  
      (Just testKeterNode) `shouldBe` (decode "{\"KeterNode\":{\"Right\":\"./\"}}")
  describe "toJSON ActiveKeterNodeId" $ do
    it "should convert to a JSON string" $ do                                  
      (encode testActiveKeterNodeId) `shouldBe` ("{\"unKID\":\"this is a test\"}")
  describe "fromJSON ActiveKeterNodeId" $ do
    it "should convert to an ActiveKeterNodeId " $ do                                  
      (decode ("{\"unKID\":\"this is a test\"}")) `shouldBe` (Just testActiveKeterNodeId)



-- |Keter Node JSON Serialization test helpers
testFilePath = ""<.> "" </> "" 
testKeterNode = KeterNode testFilePath


testActiveKeterNodeId :: ActiveKeterNodeId 
testActiveKeterNodeId = ActiveKeterNodeId "this is a test"
                        
