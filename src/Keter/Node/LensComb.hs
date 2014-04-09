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

import qualified Data.Map.Strict as M
import qualified Data.Set as S

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
n


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



-- | Pair of ISO makers to facilitate app termination

_isoAT = iso _anMapToTriple _tripleSetToANMap

_anMapToTriple :: Map KeterNodeExec (S.Set ActiveKeterNodeId) -> Set (KeterNodeExec,KeterNodeHost,KeterNodeId)
_anMapToTriple knm = M.foldlWithKey fldFcn S.empty knm 
    where fldFcn :: S.Set (KeterNodeExec,KeterNodeHost,KeterNodeId) -> KeterNodeExec -> S.Set ActiveKeterNodeId -> S.Set (KeterNodeExec,KeterNodeHost,KeterNodeId)
          fldFcn triples kne aknSet = let newTripleSet = S.map (\(ActiveKeterNodeId h i) -> (kne,KeterNodeHost h, KeterNodeId i)) aknSet
                                      in S.union newTripleSet triples
_tripleSetToANMap :: Set (KeterNodeExec,KeterNodeHost,KeterNodeId) -> Map KeterNodeExec (S.Set ActiveKeterNodeId)
_tripleSetToANMap kns = S.foldl' fldFcn M.empty kns 
    where fldFcn mp ((kne,(KeterNodeHost h),(KeterNodeId i) ))  = over (at kne) (insFcn (ActiveKeterNodeId h i))  mp
          insFcn :: ActiveKeterNodeId -> Maybe (S.Set ActiveKeterNodeId) -> Maybe (S.Set ActiveKeterNodeId)
          insFcn aknid Nothing = Just . S.insert aknid $ S.empty
          insFcn aknid (Just s) = Just . S.insert aknid $ s
