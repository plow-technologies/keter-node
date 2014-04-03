{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, 
             FlexibleInstances, NoMonomorphismRestriction, NoImplicitPrelude #-}

module Keter.Node.Lens where

import BasicPrelude 

-- Lens Specific
import Control.Lens

-- Project specific
import Keter.App
import Keter.AppManager
import Keter.Types
import Keter.Node.Types



{-|
@
data Stanza port
    = StanzaStaticFiles !StaticFilesConfig
    | StanzaRedirect !RedirectConfig
    | StanzaWebApp !(WebAppConfig port)
    | StanzaReverseProxy !ReverseProxyConfig
    | StanzaBackground !BackgroundConfig
            -- FIXME console app
    deriving Show

data BundleConfig = BundleConfig
    { bconfigStanzas :: !(Vector (Stanza ()))
    , bconfigPlugins :: !Object -- ^ settings used for plugins
    }
@

|-}

makeClassy_ ''BundleConfig 
makePrisms ''Stanza
makeClassy_ ''StaticFilesConfig
makeClassy_ ''RedirectConfig
makeClassy_ ''WebAppConfig 
makeClassy_ ''ReverseProxyConfig
makeClassy_ ''BackgroundConfig
