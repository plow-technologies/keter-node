{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Keter.Node.ConfigurationSpec (main, spec) where
import Data.Traversable.Compat
import Keter.Node
import Keter.Node.Types
import Keter.Node.Internal
import Keter.Node.Configuration
import Prelude hiding (FilePath)
import Keter.App
import Keter.AppManager
import Keter.Types
import Control.Concurrent
import Test.Hspec
import qualified Data.Vector as V

import Filesystem.Path.CurrentOS (directory, encodeString, (<.>),(</>),empty)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "startNode" $ do
    it "should create the default node tree" $ do
                           rslt <- setupNode Nothing
                           True `shouldBe` False



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
      
