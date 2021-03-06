

> {-# LANGUAGE OverloadedStrings, NoImplicitPrelude, NoRecordWildCards, DeriveGeneric #-}

> module Keter.Node.Types where




CorePrelude is not something that should stay in this library but sure is nice for development!
Control Libraries


> import CorePrelude
> import Control.Monad 
> import GHC.Generics


More standard File System, and configuration libraries

> import Data.Yaml
> import Data.Yaml.FilePath
> import Filesystem                (createTree, isFile, rename,listDirectory)
> import Filesystem.Path.CurrentOS (directory, encodeString, (<.>),(</>),empty,toText,fromText)
> import Data.Aeson
        
Good Containers to bring along

 

> import qualified Data.HashMap.Strict       as H 
> import qualified Data.Map                  as Map
> import qualified Data.Set                  as S
> import qualified Data.Vector               as V


Keter Specific stuff





> import Keter.Types
> import Keter.Main
> import Keter.App
> import Keter.AppManager
> import Data.Conduit.Process.Unix  -- Totally a keter package now
> import qualified Keter.PortPool            as PortPool
> import qualified Codec.Archive.TempTarball as TempFolder
> import qualified Keter.HostManager         as HostMan
 
 
> import Data.Default


--------------------------------------------------
--------------------------------------------------
Most of the KeterNode types are just wrappers to simplify the Keter Types for 
this library's use.

--------------------------------------------------
--------------------------------------------------
KeterNode Specific Types

a KeterNode is a file path that can be serialized, and represents a *.keter tarball for 
a binary that will make the base for a running Node.





> newtype KeterNode = KeterNode {unKeterNode :: FilePath}
>     deriving (Generic,Show,Eq,Ord)
> 
> instance ToJSON KeterNode where 
>   toJSON = (\kn -> object [label .= (obj kn)])
>     where 
>       label :: Text 
>       label = "KeterNode"
>       obj :: KeterNode -> Value
>       obj kn = case toText (unKeterNode kn) of 
>                  Right t -> toJSON t
>                  Left e -> toJSON err
>                      where err:: Text 
>                            err = "Error decoding keternode, filename failure"
>                    
> 
> instance FromJSON KeterNode where 
>     parseJSON (Object v) = KeterNode <$> 
>                            (v .: "KeterNode" >>= return.fromText)
>     parseJSON _ = fail "Expecting Object, KeterNode"


An ActiveKeterNodeId is the Text generated after a KeterNode has been put through the encoder



> data ActiveKeterNodeId = ActiveKeterNodeId { 
>                                              unKNHost :: Text,
>                                              unKID :: Int
>     } 
>     deriving (Show,Eq,Generic,Ord) 
> 
> instance ToJSON ActiveKeterNodeId where 

> instance FromJSON ActiveKeterNodeId where 

                                      


KeterNodeChan give the nodeId (for termination and status) as well as the path back to the node

> data KeterNodeChan = KeterNodeChan { knrAID :: ActiveKeterNodeId
>                                     , knrAddress :: Text}


KeterNodeConfig is where the KeterNode that have been added to an Application are stored, 
In addition to that, the Keter Config which sets up how the application will be ran is stored here too.


> data KeterNodeConfig = KeterNodeConfig { 
>        knCfgNodes :: S.Set KeterNode
>      , knCfgKeterConfig :: KeterConfig
>      , knCfgRoot :: FilePath
> }



he Keter Node Stanza Config are the configuration options keter uses when it opens up a bundle and re-writes the config file 

> data KeterNodeStanzaConfig = KeterNodeStanzaConfig { 
>          knsCfgHosts :: (S.Set KeterNodeHost)
>        , knsCfgIds :: (S.Set KeterNodeId)
>        , knsCfgExec :: (KeterNodeExec)
> } deriving (Eq,Show,Generic,Ord) 

> newtype KeterNodeExec = KeterNodeExec  { getKNExec :: Text} deriving (Eq,Show,Generic,Ord)
> newtype KeterNodeHost = KeterNodeHost  { getKNHost :: Text} deriving (Eq,Show,Generic,Ord)

This type is used with the exec filename to create the ActiveKeterNodeId, which stores
running procs

> newtype KeterNodeId = KeterNodeId  { getKNId :: Int} deriving (Eq,Show,Generic,Ord)

> instance ParseYamlFile  KeterNodeStanzaConfig where 
>     parseYamlFile basedir = withObject "keter-node" $ \o -> do
>        (hosts) <- do
>                   hs <- o .: "hosts"
>                   case hs of
>                           [] -> fail "Must provide at least one host"
>                           h:hs' -> return . S.fromList  $  KeterNodeHost <$> hs
>        (ports) <- do
>                   ns <- o .: "nodeids"
>                   case ns of
>                           [] -> fail "Must provide at least one node id"
>                           n:ns' -> return . S.fromList $  KeterNodeId <$> ns
>        (exec) <- do
>                   e <- o .: "exec"
>                   return . KeterNodeExec $ e

>        KeterNodeStanzaConfig  <$> return hosts 
>                               <*> return ports
>                               <*> return exec


> instance ToJSON KeterNodeStanzaConfig where 
>     toJSON (KeterNodeStanzaConfig hs ps e) = keterNodeObject 
>         where keterNodeObject = object ["keter-node" .= (object ["hosts" .= toJSON (getKNHost <$> S.toList hs) , "ports" .= toJSON (getKNId <$> S.toList ps),"exec" .= toJSON (getKNExec e)] )]



> instance FromJSON KeterNodeStanzaConfig where
>     parseJSON = withObject "keter-node" $ \o -> do
>                                         (hosts) <- return . S.fromList . (fmap KeterNodeHost) =<< o .: "hosts" 
>                                         (ports) <- return . S.fromList . (fmap KeterNodeId) =<< o .: "ports"
>                                         (exec) <- return . KeterNodeExec =<< o .: "exec"
>                                         KeterNodeStanzaConfig  <$> return hosts 
>                                                                <*> return ports
>                                                                <*> return exec

>                             
> instance ToJSON KeterNodeHost where
> instance ToJSON KeterNodeId where
> instance ToJSON KeterNodeExec where


> instance FromJSON KeterNodeHost where
> instance FromJSON KeterNodeId where
> instance FromJSON KeterNodeExec where

KeterNodeWatcher 
--------------------------------------------------
A Node wrapper for AppManager

Plus the lookup table for running KeterNodes by  Ket


> type KeterNodeMap = Map KeterNode (S.Set ActiveKeterNodeId)
> 
> data KeterNodeWatcher = KeterNodeWatcher { 
>        knmAppManager :: AppManager 
>      , knmAppTemp   :: TempFolder.TempFolder
>      , knmAppNodes  :: KeterNodeMap 
>      , knmCfg       :: KeterNodeConfig
>     } 



> newtype KeterNodeError = KeterNodeError { unKeterNodeError :: Text }
>     deriving (Eq,Show,Generic)
> 
> instance ToJSON KeterNodeError where 
> 
> instance FromJSON KeterNodeError where 


KeterNodeArgs wrap arguments to pass to spawned Nodes




> newtype KeterNodeArgs = KeterNodeArgs { unKeterNodeArgs :: (Vector Text ) }
>     deriving (Eq, Show, Generic) 
 
> instance ToJSON KeterNodeArgs where 
 
> instance FromJSON KeterNodeArgs where 


KeterNodeCmd is a pairing of a post route and argument body to be sent to a
KeterNode 




> data KeterNodeCmd = KeterNodeCmd { 
>       kncArgs :: Value, 
>       kncRoute :: Text
> } deriving (Show,Generic)
 
> instance ToJSON KeterNodeCmd where 
> instance FromJSON KeterNodeCmd where 
     

