{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Keter.Node.Tests ( isKeterNode)
                        
where

-- Control Libraries 
import CorePrelude

-- Local Libraries
import Keter.Node.Internal

import Keter.Node.Types


isKeterNode :: KeterNodeWatcher -> KeterNode -> IO (Either KeterNodeError KeterNode)
isKeterNode knw kn = return . Right $ kn
