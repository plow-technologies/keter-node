{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, 
             FlexibleInstances, NoMonomorphismRestriction, NoImplicitPrelude #-}

module Keter.Node.LensComb where

import BasicPrelude 

-- Lens Specific
import Control.Lens

-- Project specific
import Keter.App
import Keter.AppManager
import Keter.Types
import Keter.Node.Lens
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

data WebAppConfig port = WebAppConfig
    { waconfigExec        :: !F.FilePath
    , waconfigArgs        :: !(Vector Text)
    , waconfigEnvironment :: !(Map Text Text)
    , waconfigApprootHost :: !Text -- ^ primary host, used for approot
    , waconfigHosts       :: !(Set Text) -- ^ all hosts, not including the approot host
    , waconfigSsl         :: !Bool
    , waconfigPort        :: !port
    }
    deriving Show



makeClassy_ ''BundleConfig 
makePrisms ''Stanza
makeClassy_ ''StaticFilesConfig
makeClassy_ ''RedirectConfig
makeClassy_ ''WebAppConfig 
makeClassy_ ''ReverseProxyConfig
makeClassy_ ''BackgroundConfig
|-}



-- |Lens Combinations, shorthand.

_stanzaWebExec :: (Applicative f, HasBundleConfig t) =>
                        (FilePath -> f FilePath) -> t -> f t
_stanzaWebExec = _bconfigStanzas.traverse._StanzaWebApp._waconfigExec

_stanzaWebPort :: (Applicative f, HasBundleConfig t) =>
                        (() -> f ()) -> t -> f t
_stanzaWebPort = _bconfigStanzas.traverse._StanzaWebApp._waconfigPort

_stanzaWebHost :: (Applicative f, HasBundleConfig t) =>
                        (Text -> f Text) -> t -> f t
_stanzaWebHost = _bconfigStanzas.traverse._StanzaWebApp._waconfigApprootHost


-- | return a Set of active nodes corresponding to the keternode
_activenodestype kn = (_knmAppNodes.(at kn))
