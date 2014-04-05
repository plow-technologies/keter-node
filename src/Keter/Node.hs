{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Keter.Node (  setupNode 
                   , spawnNode 
                   , endNode
                   , listNodes )

where

-- Control Libraries 
import BasicPrelude hiding (writeFile)
import Filesystem   
import Data.Traversable
import Data.Default
import Control.Monad

import Control.Lens

-- Containers 
import qualified Data.Map as M

-- Local Libraries
import Keter.Node.Internal
import Keter.Node.Types
import Keter.Node.Tests
import Keter.Node.Configuration

-- Project specific
import Keter.App
import Keter.AppManager
import Filesystem (getWorkingDirectory)
import Filesystem.Path.CurrentOS (directory, encodeString, (<.>),(</>),empty,toText)
import qualified Codec.Archive.TempTarball as TempFolder


{-|

keter-node is designed to extend the functionality of keter to allow the same binary to be ran multiple times in a given keter directory under different port configurations.  

This allows you to spawn instances of a binary as 'nodes' and then let the spawning agent know what the port and process are for future reference 

Example Code: 

>>> do
>>> mMeterNodeWatcher <- initializeKeterNodeDefault  -- start a Keter Node server
>>> 

>>> do 
>>> keterNodeStatus <- spawnNewNode KeterNodeWatcher KeterNode Args --spawn a new node of the type KeterNode
|-}

{-| There are some important differences in the default settings from Keter,
    1. The defaultPort is set to 2066 instead of 80.
|-}

-- | Start KeterNodeConfig with an incoming Configuration or leave that off and a default set of options will be used
-- Unlike keter, the Node enviornment always starts empty

setupNode :: (Maybe KeterNodeConfig) -> IO (Either KeterNodeError KeterNodeWatcher)
setupNode Nothing = setupNode' def
setupNode (Just knc) = setupNode' knc

setupNode' :: KeterNodeConfig -> IO (Either KeterNodeError KeterNodeWatcher) 
setupNode' knc = do 
  wd <- getWorkingDirectory
  unless ( wd == (knCfgRoot knc))  (createDirectory True (knCfgRoot knc) >> print "creating root")
  setWorkingDirectory (knCfgRoot knc)
  void $ traverse createTree defaultPaths
  tmpFolder <- TempFolder.setup $ tmpFilePath
  asc <- simpleWatcher
  appMan <- initialize (\_ -> print ("log"::String))  asc   
  return $ Right (KeterNodeWatcher {  knmAppManger = appMan 
                                    , knmAppNodes = M.empty 
                                    , knmAppTemp  = tmpFolder })
  
-- |Spawn a new node by creating a copy of the *.keter file represented by Keter Node, parsing all the config files and 
-- encoding the binary, then copying the tarball into it's own directory to run.
-- The KeterNodeManger and the KeterNodeId are returned

-- The KeterNode file is extracted into a temp directory, the relevant files are interpolated and then re-bundled into a new *.keter file which represents the spawned node
-- spawnNode :: KeterNodeWatcher -> KeterNode -> KeterNodeArgs -> IO (Either KeterNodeError (KeterNodeWatcher,KeterNodeChan))
spawnNode knw kn knargs = isKeterNode knw kn >>= (\ekn -> do 
                                                    let  etup = (\kn fp -> (kn,fp)) <$> ekn <*> (over _Left (\_ -> (KeterNodeError "invalid node-type fp")) (toText nodeTypeFilePath) )
                                                    case etup of 
                                                      (Left e) ->  return $ Left e
                                                      (Right (kn,fp) ) -> do 
                                                               tt <- TempFolder.unpackTempTar 
                                                                     Nothing                                                                        
                                                                     (knmAppTemp knw)
                                                                     (nodeTypeFilePath </> (unKeterNode kn ))
                                                                     fp 
                                                                     buildKeterNode
                                                               return $ Right () )                                                                                                                                                                                      


-- | Grab a map with a singleKey, given by the KeterNode, containing all running instances of a KeterNode
--   Leave the Key out to return all the Nodes running
listNodes :: KeterNodeWatcher -> (Maybe KeterNode) -> IO (Either KeterNodeError (KeterNodeMap) )
listNodes = undefined
            
endNode :: KeterNodeWatcher -> ActiveKeterNodeId -> KeterNodeCmd -> IO (Either KeterNodeError ())
endNode = undefined

-- Just put the Nothing in
-- initializeKeterNOdeDef :: IO

