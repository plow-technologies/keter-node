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
import qualified Data.Set as S

-- Local Libraries
import Keter.Node.Internal
import Keter.Node.Types
import Keter.Node.Tests
import Keter.Node.Configuration
import Keter.Node.Lens
import Keter.Node.LensComb

-- Project specific
import Keter.App
import Keter.AppManager
import Filesystem (getWorkingDirectory)
import Filesystem.Path.CurrentOS (directory, encodeString, (<.>),(</>),empty,toText)
import qualified Codec.Archive.TempTarball as TempFolder


{-|

keter-node is designed to extend the functionality of keter to allow the same binary to be ran multiple times in a gigven keter directory under different port configurations.  

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
  appMan <- initialize (\l -> (print "Working Directory" >>= print) >> print "logmsg">> print l )  asc   
  return $ Right (KeterNodeWatcher {  knmAppManager = appMan 
                                    , knmAppNodes = M.empty 
                                    , knmAppTemp  = tmpFolder
                                    , knmCfg      = knc })
  
-- |Spawn a new node by creating a copy of the *.keter file represented by Keter Node, parsing all the config files and 
-- encoding the binary, then copying the tarball into it's own directory to run.
-- The KeterNodeManger and the KeterNodeId are returned

-- The KeterNode file is extracted into a temp directory, the relevant files are interpolated and then re-bundled into a new *.keter file which represents the spawned node

spawnNode :: KeterNodeWatcher -> KeterNode -> t -> IO (Either KeterNodeError KeterNodeWatcher)
spawnNode knw kn knargs = isKeterNode knw kn >>= (\ekn -> do 
                                                    wd <- getWorkingDirectory
                                                    let  etup = (\kn fp -> (kn,fp)) <$> ekn <*> (over _Left (\_ -> (KeterNodeError "invalid node-type fp")) (toText nodeTypeFilePath) )
                                                    case etup of 
                                                      (Left e) ->  return $ Left e
                                                      (Right (kn,fp) ) -> do 
                                                               et <- TempFolder.unpackTempTar 
                                                                     Nothing                                                                        
                                                                     (knmAppTemp knw)
                                                                     (nodeTypeFilePath </> (unKeterNode kn ))
                                                                     fp 
                                                                     (buildKeterNode knw kn)
                                                               print "changing working directory to..." >> print wd 
                                                               setWorkingDirectory wd
                                                               print "add app"
                                                               void $ (\(app,_) -> addApp (knmAppManager knw) (wd </> activeNodes </> app) ) `traverse` et
                                                               print ("added file --> "::Text) >> print et
                                                               et' <- return (over _Left (\e -> KeterNodeError e ) et) 
                                                               setWorkingDirectory wd
                                                               return $ (\(_,i) -> over (_activenodestype kn) (sFcn i) knw) <$> et' )
                                                                   where 
                                                                     sFcn :: ActiveKeterNodeId -> Maybe (S.Set ActiveKeterNodeId)  -> Maybe (S.Set ActiveKeterNodeId)
                                                                     sFcn aknid Nothing = Just ( S.insert  aknid S.empty) 
                                                                     sFcn aknid (Just s) = Just ( S.insert  aknid s) 



-- | Grab a map with a singleKey, given by the KeterNode, containing all running instances of a KeterNode
--   Leave the Key out to return all the Nodes running
listNodes :: KeterNodeWatcher -> (Maybe KeterNode) -> IO (Either KeterNodeError (KeterNodeMap) )
listNodes = undefined
            
endNode :: KeterNodeWatcher -> ActiveKeterNodeId -> KeterNodeCmd -> IO (Either KeterNodeError ())
endNode = undefined

-- Just put the Nothing in
-- initializeKeterNOdeDef :: IO

