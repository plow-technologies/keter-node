{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module Keter.NodeSpec (main, spec) where
import Data.Traversable
import Data.Default
import Keter.Node
import Keter.Node.Types
import Keter.Node.Internal
import Prelude hiding (FilePath)
import Keter.App
import Keter.AppManager
import Keter.Types
import qualified Shelly as Sh
import qualified Data.Set as S
import Control.Concurrent
import Test.Hspec
import Control.Applicative
import qualified Data.Vector as V
import Filesystem (getWorkingDirectory,isFile,setWorkingDirectory,listDirectory)
import Filesystem.Path.CurrentOS (directory, encodeString, (<.>),(</>),empty,dirname,filename)
    
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "startNode" $ do
    it "should create the default node tree" $ do
                           rslt <- setupNode Nothing
                           wd <- getWorkingDirectory 
                           putStrLn "workingDir " >> print wd
                           dirs <- listDirectory wd 
                           let dirSet = S.fromList (filename <$> dirs )
                               targetSet = S.fromList defaultPaths
                               intersection = S.intersection dirSet targetSet                                              
                           print dirSet
                           (S.null intersection) `shouldBe` False

-- testAppManager :: IO AppManager
-- testAppManager = do 
--   asc <- simpleWatcher
--   initialize (\_ -> print ("log"::String))  asc   b
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
  getWorkingDirectory >>= print
  eeknw <- (traverse tFcn eknw )
  return $ (eeknw >>= (\eknw -> eknw))
    where 
      tFcn :: KeterNodeWatcher -> IO (Either KeterNodeError KeterNodeWatcher)
      tFcn = tFcn' kn kna 
      kn = KeterNode $ "testproc" <.> "keter"
      kna = KeterNodeArgs (V.fromList ["-p","3030"])
      tFcn' = flip.flip spawnNode

testSpawnSecondNode knw = spawnNode knw (KeterNode $ "impulse-node" <.> "keter") (KeterNodeArgs V.empty) 


-- | Hard coded start node 
testStartNode = do 
  eknw <- setupNode Nothing 
  print "setupDone" >> getWorkingDirectory >>= print
  traverse tFcn eknw 
    where 
      tFcn :: KeterNodeWatcher -> IO () -- Either KeterNodeError FilePath)
      tFcn knw = addApp (knmAppManager knw) ("/home/scott/programs/src/keter-node/keter-node-root/active-nodes/impulse-node.keter") 
      kn = KeterNode $ "test-proc" <.> "keter"
      kna = KeterNodeArgs V.empty

testEndNode = do 
  tl <- testSpawnNode
  threadDelay 10000000 >>  traverse (flip endNode Nothing ) tl 
