
\begin{code}

{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, NoRecordWildCards, DeriveGeneric #-}
module Keter.Node.Types where



\end{code}

CorePrelude is not something that should stay in this library but sure is nice for development!
Control Libraries

\begin{code}

import CorePrelude
import Control.Monad 
import GHC.Generics
\end{code}


More standard File System, and configuration libraries

\begin{code}

import Data.Yaml
import Data.Yaml.FilePath
import Filesystem                (createTree, isFile, rename)
import Filesystem.Path.CurrentOS (directory, encodeString, (<.>),(</>),empty)
import Data.Aeson

\end{code}
        
Good Containers to bring along

\begin{code} 

import qualified Data.HashMap.Strict       as H 
import qualified Data.Map                  as Map
import qualified Data.Set                  as S
import qualified Data.Vector               as V

\end{code}

Keter Specific stuff



\begin{code} 

import Keter.Types
import Keter.Main
import Keter.App
import Data.Conduit.Process.Unix  -- Totally a keter package now
import qualified Keter.PortPool            as PortPool
import qualified Codec.Archive.TempTarball as TempFolder
import qualified Keter.HostManager         as HostMan


import Data.Default

\end{code}

--------------------------------------------------
--------------------------------------------------

KeterNode Specific Types

\begin{code}




\end{code}



Empty and default declarations for various Keter Objects used to create nodes, 
most notably the BackgroundConfig

\begin{code}
emptyTempFolder :: IO TempFolder.TempFolder 
emptyTempFolder = TempFolder.setup   $ (kconfigDir emptyKeterConfig) </> "temp" 

emptyPortPool :: PortSettings 
emptyPortPool = PortSettings [] 

emptyPlugins :: [Plugin]
emptyPlugins = [] 


defaultPort :: NonEmptyVector ListeningPort
defaultPort =  NonEmptyVector (LPInsecure "*" 3000) V.empty

emptyKeterConfig :: KeterConfig
emptyKeterConfig =  KeterConfig
        { kconfigDir = "./keterTest"
        , kconfigPortPool = emptyPortPool
        , kconfigListeners = defaultPort
        , kconfigSetuid = Nothing
        , kconfigBuiltinStanzas = V.empty
        , kconfigIpFromHeader = False
        }


simpleManager :: IO AppStartConfig
simpleManager = do 
  processTracker <- initProcessTracker
  tf <-  emptyTempFolder 
  hostman <- HostMan.start
  portpool <- PortPool.start emptyPortPool
  let appStartConfig = AppStartConfig
                       { ascTempFolder = tf
                       , ascSetuid = Nothing
                       , ascProcessTracker = processTracker
                       , ascHostManager = hostman
                       , ascPortPool = portpool
                       , ascPlugins = emptyPlugins
                       , ascLog = (\_ -> return () ) 
                       , ascKeterConfig = emptyKeterConfig
                       }
  return appStartConfig



\end{code}

Note Bundle Config looks like this 

``` haskell
instance ToJSON BundleConfig -- Defined in `Keter.Types.V10'
instance ParseYamlFile BundleConfig -- Defined in `Keter.Types.V10'
instance ToCurrent BundleConfig -- Defined in `Keter.Types.V10'
```

\begin{code}

sampleBundleConfig :: BundleConfig
sampleBundleConfig = BundleConfig (V.fromList [defStanza]) H.empty
            
\end{code}

\begin{code} 
         
defFP :: FilePath
defFP = ""<.> "" </> "" 

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

\end{code}

