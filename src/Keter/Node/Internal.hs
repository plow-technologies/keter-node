{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, NoRecordWildCards, DeriveGeneric #-}
module Keter.Node.Internal
    (
     defaultKeterConfig 
    ,simpleWatcher
    , defaultPaths
    , sampleBundleConfig
    , activeNodes
    , tmpFilePath
    , nodeTypeFilePath
    ) where


-- Control
import CorePrelude
-- import Control.Monad 
-- import Filesystem   (createTree, isFile, rename,listDirectory,setWorkingDirectory)
-- import Filesystem.Path.CurrentOS 
-- Containers

import qualified Data.HashMap.Strict       as H 
import qualified Data.Map                  as Map
import qualified Data.Set                  as S
import qualified Data.Vector               as V

-- Conversion
-- import Data.Yaml
import Data.Yaml.FilePath


-- Keter Specific

import Keter.Node.Types
import Keter.Types
-- import Keter.Main
import Keter.App
-- import Keter.AppManager
import Data.Conduit.Process.Unix  -- Totally a keter package now
import qualified Keter.PortPool            as PortPool
import qualified Codec.Archive.TempTarball as TempFolder
import qualified Keter.HostManager         as HostMan


import Data.Default


-- ==================================================
-- Keter Node Specific 


-- | active-nodes are those that are running
-- keter-node-logs... Log info having to do with this library 
-- node-types... incoming .keter files which could be spawned

defaultPaths :: [FilePath]
defaultPaths = [an,knl,nt,tmp] 
    where
      f t = "" <.> "" </> t
      an  = activeNodes
      knl = f "keter-node-logs"
      nt  = nodeTypeFilePath
      tmp = tmpFilePath


activeNodes :: FilePath
activeNodes = "active-nodes"                 


tmpFilePath :: FilePath
tmpFilePath = "node-temp"

nodeTypeFilePath :: FilePath
nodeTypeFilePath = "node-types"
--------------------------------------------------

-- Defaults For the simpleWatcher
--------------------------------------------------

defaultTempFolder :: IO TempFolder.TempFolder 
defaultTempFolder = TempFolder.setup   $ (kconfigDir defaultKeterConfig) </> "temp" 

defaultPort :: NonEmptyVector ListeningPort
defaultPort =  NonEmptyVector (LPInsecure "*" 2066) V.empty

defaultKeterConfig :: KeterConfig
defaultKeterConfig =  KeterConfig
        { kconfigDir = activeNodes
        , kconfigPortPool = emptyPortPool
        , kconfigListeners = defaultPort
        , kconfigSetuid = Nothing
        , kconfigBuiltinStanzas = V.empty
        , kconfigIpFromHeader = False
        }

-- | SimpleWatcher tries to create the Keter AppStartConfig type 
-- with as little info as possible
simpleWatcher :: IO AppStartConfig
simpleWatcher = do 
  processTracker <- initProcessTracker
  tf <-  defaultTempFolder 
  hostman <- HostMan.start
  portpool <- PortPool.start emptyPortPool
  let appStartConfig = AppStartConfig
                       { ascTempFolder = tf
                       , ascSetuid = Nothing
                       , ascProcessTracker = processTracker
                       , ascHostManager = hostman
                       , ascPortPool = portpool
                       , ascPlugins = emptyPlugins
                       , ascLog = (\l -> print l ) 
                       , ascKeterConfig = defaultKeterConfig                       
                       }
  return appStartConfig

{-|


Note Bundle Config looks like this 

``` haskell
instance ToJSON BundleConfig -- Defined in `Keter.Types.V10'
instance ParseYamlFile BundleConfig -- Defined in `Keter.Types.V10'
instance ToCurrent BundleConfig -- Defined in `Keter.Types.V10'
```
|-}

sampleBundleConfig :: BundleConfig
sampleBundleConfig = BundleConfig (V.fromList [defStanza]) H.empty


emptyPortPool :: PortSettings 
emptyPortPool = PortSettings []

emptyPlugins :: [Plugin]
emptyPlugins = [] 

            
         
defFP :: FilePath
defFP = "keter-node-root" 

defStanza :: Stanza port
defStanza = StanzaBackground (defBackgroundConfig defFP)

defBackgroundConfig :: FilePath -> BackgroundConfig
defBackgroundConfig fp = BackgroundConfig
                      { bgconfigExec = fp
                      , bgconfigArgs = V.empty
                      , bgconfigEnvironment = Map.empty
                      , bgconfigRestartCount = LimitedRestarts 2
                      , bgconfigRestartDelaySeconds = 10
                      }



instance Default KeterNodeConfig where 
    def = KeterNodeConfig {
            knCfgNodes = S.empty
          , knCfgKeterConfig = defaultKeterConfig
          , knCfgRoot = "" <.>"" </>"keter-node-root"
          }
