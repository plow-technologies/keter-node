{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Keter.NodeSpec (main, spec) where
import Data.Traversable
import Keter.Node
import Keter.Node.Types
import Keter.Node.Internal
import Prelude hiding (FilePath)
import Keter.App
import Keter.AppManager
import Keter.Types
import qualified Shelly as Sh
import Control.Concurrent
import Test.Hspec
import qualified Data.Vector as V
import Filesystem (getWorkingDirectory,isFile,setWorkingDirectory)
import Filesystem.Path.CurrentOS (directory, encodeString, (<.>),(</>),empty)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "startNode" $ do
    it "should create the default node tree" $ do
                           rslt <- setupNode Nothing
                           True `shouldBe` False



-- testAppManager :: IO AppManager
-- testAppManager = do 
--   asc <- simpleWatcher
--   initialize (\_ -> print ("log"::String))  asc   
-- testFP :: FilePath
-- testFP = "" <.> ""</> "toyproc" <.> "keter"

-- testManagerUse :: IO () 
-- testManagerUse = do 
--   apmgr <- testAppManager 
--   addApp apmgr testFP
--   addApp apmgr testFP
--   print ("App Created!"  :: String)
--   threadDelay 10000000 >> terminateApp apmgr ("toyproc") >> print ("app terminated" :: String)


testSpawnNode = do 
  eknw <- setupNode Nothing 
  print "setupDone"
  traverse tFcn eknw 
    where 
      tFcn :: KeterNodeWatcher -> IO (Either KeterNodeError ())
      tFcn = tFcn' kn kna
      kn = KeterNode $ "impulse-node" <.> "keter"
      kna = KeterNodeArgs V.empty
      tFcn' = flip.flip spawnNode

