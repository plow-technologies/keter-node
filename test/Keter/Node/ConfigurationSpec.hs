{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Keter.Node.ConfigurationSpec (main, spec) where
import Data.Traversable.Compat
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

import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Set as S
import Filesystem 
import Filesystem.Path.CurrentOS 

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "startNode" $ do
    it "should create the default node tree" $ do
                           rslt <- setupNode Nothing
                           True `shouldBe` False
  describe "decodeKeterNodeStanzaConfig" $ do
    it "should decode a yaml file to a KeterNodeStanzaConfig" $ do
                           rslt <- decodeKeterNodeStanzaConfig testConfigFile
                           case rslt of 
                             Left _ -> False `shouldBe` True
                             Right k -> do 
                                       k `shouldBe` expectedKeterNodeStanzaConfig


-- | simple test spawn node for doing configuration work. 
testSpawnNode = do 
  eknw <- setupNode Nothing 
  traverse tFcn eknw 
    where 
      tFcn :: KeterNodeWatcher -> IO (Either KeterNodeError ())
      tFcn = tFcn' kn kna
      kn = KeterNode $ "impulse-node" <.> "keter"
      kna = KeterNodeArgs V.empty
      tFcn' = flip.flip spawnNode
      
expectedKeterNodeStanzaConfig = KeterNodeStanzaConfig (S.fromList (KeterNodeHost <$> ["www.aacs-us.com","www.plowtech.net"]) ) (S.fromList (KeterNodePort <$> [1000,1001]))

testConfigFile = "" <.> "" </> "temp" </> "config" </> "keter-node" <.> "yaml"
