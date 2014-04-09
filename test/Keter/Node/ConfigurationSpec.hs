{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Keter.Node.ConfigurationSpec (main, spec) where

import Keter.Node
import Keter.Node.Types
import Keter.Node.Internal
import Keter.Node.Configuration
import Control.Applicative
import Data.Yaml
import Prelude hiding (FilePath)
import Keter.App
import Keter.AppManager
import Keter.Types
import Control.Concurrent
import Test.Hspec
import Data.Traversable
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Set as S
import Filesystem 
import Filesystem.Path.CurrentOS 

main :: IO ()
main = hspec spec

spec :: Spec
spec = do  
  describe "knSuffixFilename" $ do 
         it "should append a host and id to the file" $ do 
                                  let rslt = testSuffixFilename
                                      comp = testFilename ::Text
                                  rslt `shouldBe` comp
  describe "knUnSuffixFileName" $ do 
         it "should take 2 of the 3 keys and a text string and return a triple with all of the keys" $ do                                     
                                  let rslt1 = knUnSuffixFileName (Just tKExec) (Just tKHost) Nothing t
                                      rslt2 = knUnSuffixFileName (Just tKExec) (Just tKHost) (Just tKId) t
                                      rslt3 = knUnSuffixFileName (Just tKExec) Nothing (Just tKId) t
                                      rslt4 = knUnSuffixFileName Nothing (Just tKHost) (Just tKId) t
                                      rslt5 = knUnSuffixFileName Nothing Nothing (Just tKId) t
                                      t = testFilename
                                      rslt = and $ fmap (== (Right (tKExec,tKHost,tKId))) [rslt1,rslt2,rslt3,rslt4]
                                  rslt `shouldBe` True
                                  ((\(Left r) -> True ) rslt5) `shouldBe` True





testSuffixFilename = knSuffixFilename tKExec tKHost tKId

testFilename = "impulse-nodelocalhost3000"
tKExec = KeterNodeExec "impulse-node"                                       
tKHost = KeterNodeHost "localhost"
tKId   = KeterNodeId   3000

-- | simple test spawn node for doing configuration work. 

testSpawnNode = do 
  eknw <- setupNode Nothing 
  print "setupDone"
  getWorkingDirectory >>= print
  eeknw <- (traverse tFcn eknw )
  return $ (eeknw >>= (\eknw -> eknw))
    where 
      tFcn :: KeterNodeWatcher -> IO (Either KeterNodeError KeterNodeWatcher)
      tFcn = tFcn' kn kna 
      kn = KeterNode $ "impulse-node" <.> "keter"
      kna = KeterNodeArgs (V.fromList ["-p","3030"])
      tFcn' = flip.flip spawnNode
      
expectedKeterNodeStanzaConfig = KeterNodeStanzaConfig (S.fromList (KeterNodeHost <$> ["www.aacs-us.com","www.plowtech.net"]) ) (S.fromList (KeterNodeId <$> [1000,1001]) ) (KeterNodeExec "TestApp")

testConfigFile ::FilePath
testConfigFile = "" <.> "" </> "temp" </> "config" </> "keter-node" <.> "yaml"
