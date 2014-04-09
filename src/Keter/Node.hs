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
import Filesystem (getWorkingDirectory,listDirectory)

import Filesystem.Path.CurrentOS (directory, encodeString, (<.>),(</>),empty,toText,dirname,replaceExtension)
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
setupNode Nothing = getWorkingDirectory >>= (\wd -> setupNode' def{knCfgRoot = wd </> "keter-node-root"}) 
setupNode (Just knc) = setupNode' knc

setupNode' :: KeterNodeConfig -> IO (Either KeterNodeError KeterNodeWatcher) 
setupNode' knc = do 
  wd <- getWorkingDirectory
  unless ( (wd) == (knCfgRoot $ knc))  (createDirectory True (wd </> (knCfgRoot knc)) >> print "creating root")
  setWorkingDirectory (knCfgRoot knc)
  wd' <- getWorkingDirectory
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
                                                               setWorkingDirectory wd
                                                               void $ (\(app,_) -> addApp (knmAppManager knw) (wd </> activeNodes </> app) ) `traverse` et
                                                               et' <- return (over _Left (\e -> KeterNodeError e ) et) 
                                                               return $ (\(_,i) -> over (_activenodestype kn) (sFcn i) knw) <$> et' )
                                                                   where 
                                                                     sFcn :: ActiveKeterNodeId -> Maybe (S.Set ActiveKeterNodeId)  -> Maybe (S.Set ActiveKeterNodeId)
                                                                     sFcn aknid Nothing = Just ( S.insert  aknid S.empty) 
                                                                     sFcn aknid (Just s) = Just ( S.insert  aknid s) 



-- | Grab a map with a singleKey, given by the KeterNode, containing all running instances of a KeterNode
--   Leave the Key out to return all the Nodes running
listNodes :: KeterNodeWatcher -> KeterNode -> Maybe (S.Set ActiveKeterNodeId)
listNodes knw  kn =  view (_activenodestype kn) knw


-- | Pass a Keter Node to kill all running Processes of that type
-- | Pass a Nothing to killall processes of all types
-- endNode :: KeterNodeWatcher -> (Maybe KeterNode) -> IO (Either KeterNodeError ())
endNode knw (Just kn) = do 
  let ekn =(toText.unKeterNode $ kn)
      mknset = view (_activenodestype kn) knw
      
  (\fp -> terminateApp (knmAppManager knw) fp ) `traverse` (toText.unKeterNode $ kn)
  return () --  over _Left (\e -> KeterNodeError e) er 
endNode knw Nothing = do 
  let mp' = M.mapKeys (KeterNodeExec . either (\_ -> "") id . toText . (flip replaceExtension "out") . unKeterNode) (knmAppNodes knw) :: (M.Map KeterNodeExec (S.Set ActiveKeterNodeId))
      mt  = (S.map knSuffixTuple)._anMapToTriple $ mp' :: (S.Set Text)
  print "list map" >> print mt 
  void $ traverse (terminateApp (knmAppManager knw))  (S.toList mt) 
    where 
      eFcn (Left (KeterNodeError e )) (Left e') = Left . KeterNodeError $ (e <> e')
      eFcn (Right _) (Left e') = Left . KeterNodeError $ e' 
      eFcn a (Right _) = a 
--   return $ over _Left (\e -> KeterNodeError e) e_ 

-- Just put the Nothing in
-- initializeKeterNOdeDef :: IO

