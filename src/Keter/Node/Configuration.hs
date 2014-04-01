{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Keter.Node.Configuration ( buildKeterNode
                                ) where

-- Control Libraries 
import BasicPrelude hiding (writeFile)
import Filesystem   
import Data.Traversable
import Data.Default
import Control.Monad
-- Containers 
import qualified Data.Map as M

-- Local Libraries
import Keter.Node.Internal
import Keter.Node.Types
import Keter.Node.Tests


-- Project specific
import Keter.App
import Keter.AppManager
import Filesystem (getWorkingDirectory)
import Filesystem.Path.CurrentOS (directory, encodeString, (<.>),(</>),empty)
import qualified Codec.Archive.TempTarball as TempFolder


myWDFcn fp = do
  wd <- getWorkingDirectory
  let newWd = wd </> fp
  print newWd 


buildKeterNode = myWDFcn
