# KeterNode

``` {.sourceCode .literate .haskell}
{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, NoRecordWildCards, DeriveGeneric #-}
```

``` {.sourceCode .literate .haskell}
module Keter.Node.Types 
( KeterNodeConfig 
, KeterNodeMap
, KeterNode (..)
, KeterNodeError (..)
, KeterNodeArgs (..) 
, ActiveKeterNodeId (..) 

, knCfgNodes 
, knCfgKeterConfig
, KeterNodeManager
, knmAppManger
, knmAppNodes

, KeterNodeChan
, knrAID
, knrAddress 

, KeterNodeCmd
, kncArgs 
, kncRoute 

) where
```

CorePrelude is not something that should stay in this library but sure is nice for development! Control Libraries

``` {.sourceCode .literate .haskell}
import CorePrelude
import Control.Monad 
import GHC.Generics
```

More standard File System, and configuration libraries

``` {.sourceCode .literate .haskell}
import Data.Yaml
import Data.Yaml.FilePath
import Filesystem                (createTree, isFile, rename,listDirectory)
import Filesystem.Path.CurrentOS (directory, encodeString, (<.>),(</>),empty,toText,fromText)
import Data.Aeson
       
```

Good Containers to bring along

``` {.sourceCode .literate .haskell}


import qualified Data.HashMap.Strict       as H 
import qualified Data.Map                  as Map
import qualified Data.Set                  as S
import qualified Data.Vector               as V
```

Keter Specific stuff

``` {.sourceCode .literate .haskell}
import Keter.Node.Internal
import Keter.Types
import Keter.Main
import Keter.App
import Keter.AppManager
import Data.Conduit.Process.Unix  -- Totally a keter package now
import qualified Keter.PortPool            as PortPool
import qualified Codec.Archive.TempTarball as TempFolder
import qualified Keter.HostManager         as HostMan


import Data.Default
```

--------------------------------------------------
--------------------------------------------------

Most of the KeterNode types are just wrappers to simplify the Keter Types for this library's use.

--------------------------------------------------
--------------------------------------------------

KeterNode Specific Types

a KeterNode is a file path that can be serialized, and represents a \*.keter tarball for a binary that will make the base for a running Node.

``` {.sourceCode .literate .haskell}
newtype KeterNode = KeterNode {unKeterNode :: FilePath}
    deriving (Generic,Show,Eq)

instance ToJSON KeterNode where 
  toJSON = (\kn -> object [label .= (obj kn)])
    where 
      label :: Text 
      label = "KeterNode"
      obj :: KeterNode -> Value
      obj kn = toJSON $ toText (unKeterNode kn)
                   

instance FromJSON KeterNode where 
    parseJSON (Object v) = KeterNode <$> 
                           (v .: "KeterNode" >>= return.fromText)
```

An ActiveKeterNodeId is the Text generated after a KeterNode has been put through the encoder

``` {.sourceCode .literate .haskell}
newtype ActiveKeterNodeId = ActiveKeterNodeId { unKID :: Text } 
    deriving (Show,Eq,Generic) 

instance ToJSON ActiveKeterNodeId where 
instance FromJSON ActiveKeterNodeId where 
```

KeterNodeChan give the nodeId (for termination and status) as well as the path back to the node

``` {.sourceCode .literate .haskell}
data KeterNodeChan = KeterNodeChan { knrAID :: ActiveKeterNodeId
                                    , knrAddress :: Text}
```

KeterNodeConfig is where the KeterNode that have been added to an Application are stored, In addition to that, the Keter Config which sets up how the application will be ran is stored here too.

``` {.sourceCode .literate .haskell}
data KeterNodeConfig = KeterNodeConfig { 
       knCfgNodes :: S.Set KeterNode
     , knCfgKeterConfig :: KeterConfig
     , knCfgRoot :: FilePath
}
```

instance Default KeterNodeConfig where def = KeterNodeConfig { knCfgNodes = S.empty , knCfgKeterConfig = emptyKeterConfig , knCfgRoot = "" \<.\>"" \</\>"keter-node-root" }

KeterNodeManager
----------------

A Node wrapper for AppManager

Plus the lookup table for running KeterNodes by Ket

``` {.sourceCode .literate .haskell}
type KeterNodeMap = Map KeterNode (S.Set ActiveKeterNodeId)

data KeterNodeManager = KeterNodeManager { 
       knmAppManger :: AppManager 
     , knmAppNodes  :: KeterNodeMap 
    } 
```

``` {.sourceCode .literate .haskell}
newtype KeterNodeError = KeterNodeError { unKeterNodeError :: Text }
    deriving (Eq,Show,Generic)

instance ToJSON KeterNodeError where 

instance FromJSON KeterNodeError where 
```

KeterNodeArgs wrap arguments to pass to spawned Nodes

``` {.sourceCode .literate .haskell}
newtype KeterNodeArgs = KeterNodeArgs { unKeterNodeArgs :: (Vector Text ) }
    deriving (Eq, Show, Generic) 

instance ToJSON KeterNodeArgs where 

instance FromJSON KeterNodeArgs where 
```

KeterNodeCmd is a pairing of a post route and argument body to be sent to a KeterNode

``` {.sourceCode .literate .haskell}
data KeterNodeCmd = KeterNodeCmd { 
      kncArgs :: Value, 
      kncRoute :: Text
}deriving (Show,Generic)

instance ToJSON KeterNodeCmd where 
instance FromJSON KeterNodeCmd where 
```
