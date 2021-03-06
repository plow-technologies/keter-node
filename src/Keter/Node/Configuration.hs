{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Keter.Node.Configuration ( buildKeterNode, decodeKeterNodeStanzaConfig,knSuffixFilename
                                  ,knUnSuffixFileName,knSuffixTuple 
                                ) where

-- Control Libraries 
import BasicPrelude 
import Data.Traversable
-- import Data.Default
-- import Control.Monad
import qualified Control.Lens as L 
import qualified Data.Set.Lens as L

-- System Libraries 
import Filesystem (getWorkingDirectory,isFile,setWorkingDirectory)
import Filesystem.Path.CurrentOS (fromText,toText,collapse) -- ,directory, encodeString, empty,directory,filename,(<.>),(</>))
-- import qualified Codec.Archive.TempTarball as TempFolder
import qualified Shelly as Sh

-- MonoMorphs
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.Lazy as TL

-- Parsing
import Data.Yaml
import Data.Yaml.FilePath
import Data.Text.Template


-- Containers 
import qualified Data.Map.Strict as M
import qualified Data.Set as S


-- Project specific
--import Keter.App
-- import Keter.AppManager
-- import Keter.Types


-- Local Libraries
import Keter.Node.Internal
import Keter.Node.Types
-- import Keter.Node.Tests
import Keter.Node.LensComb


decodeKeterNodeStanzaConfig :: FilePath -> IO ( Either ParseException KeterNodeStanzaConfig)
decodeKeterNodeStanzaConfig = decodeFileRelative   

-- decodeKeterStanzaConfig :: FilePath -> IO (Either ParseException BundleConfig) 
-- decodeKeterStanzaConfig = decodeFileRelative

-- | buildKeterNode parses the keter-node.yaml file and uses it to modify the keter.yaml file 
-- put the file in the config directory, right next to the keter.yaml file 

getNextId :: KeterNodeWatcher -> KeterNode -> KeterNodeHost-> Set KeterNodeId -> KeterNodeId
getNextId knw kn knh all' =  minNode
    where minNode :: KeterNodeId
          minNode = S.findMin inactive 
          inactive = S.difference all' common
          common = S.intersection all' (L.over L.setmapped (\(ActiveKeterNodeId knh k )-> KeterNodeId k) active )
          active = fromMaybe S.empty (L.view (_activenodestype kn)  knw)
          

 -- | Create 'Context' from association list.
buildKeterNode :: KeterNodeWatcher -> KeterNode -> FilePath -> IO (Either Text (FilePath,ActiveKeterNodeId))
buildKeterNode knw kn fp = do
  wd <- getWorkingDirectory
  let newWd = wd </> fp
      config = newWd </> "config" 
      knfilePth = config </> "keter-node" <.> "yaml"
      kfilePth = config </> "keter" <.> "yaml"
  putStrLn "Done Getting Directories"
  eKNSC <- decodeKeterNodeStanzaConfig knfilePth  
  setWorkingDirectory newWd 
  case eKNSC of 
    Left e -> return . Left . show $ e
    Right knf -> do 
--               print ("before append "::Text) >> (print $ getKNExec . knsCfgExec $ knf)
               let e = knsCfgExec $ knf 
                   h = S.findMin . knsCfgHosts $ knf 
                   p = getNextId knw kn h (knsCfgIds $ knf)
                   h' = getKNHost $ h -- convert everything to text 
                   p' = show.getKNId $ p
                   e' = knSuffixTuple (e, h, p) -- T.append (T.append (getKNExec e) p') h' --make new executable nameg
                   mp = M.fromList [("host",h' )  -- Build the lookup map
                                   ,("nodeid" ,p')
                                   ,("exec" , T.append "../" $ e')] 
                   context :: ContextA Maybe
                   context t = M.lookup t mp
               kfile <- readFile kfilePth
--               putStrLn "To the substituteA case"
               case substituteA kfile context of 
                 Nothing -> error "No File Read or invalid template var" 
                 Just txt -> do                      
                      writeFile kfilePth (TL.toStrict txt)
--                      putStrLn "Done writing files"
                      enkf <- onException (rewriteNodeBinary wd knf e') (return . Left $ ("Error"::Text))
                      setWorkingDirectory wd 
                      return $ (\nkf -> (nkf, (ActiveKeterNodeId h'). getKNId $ p)) <$> enkf 
--                      (rewriteNodeBinary wd knf e' >> putStrLn "Done rewriting exec") 


-- | add a suffix to the executable name 
knSuffixFilename ::  KeterNodeExec -> KeterNodeHost -> KeterNodeId -> Text 
knSuffixFilename  (KeterNodeExec e) (KeterNodeHost h) (KeterNodeId i) = T.concat [e,h,show i]


knSuffixTuple (e,h,i) = knSuffixFilename e h i
-- |any 2 of the 3 items to retrieve the triple , expects non extensioned 'pure' name
-- Runs as many checks as it can but when working with an incomplete case, that is limited
knUnSuffixFileName ::  (Maybe KeterNodeExec) -> (Maybe KeterNodeHost) -> (Maybe KeterNodeId) -> Text -> Either KeterNodeError (KeterNodeExec,KeterNodeHost,KeterNodeId)
knUnSuffixFileName  (Just ke@(KeterNodeExec e)) (Just kh@(KeterNodeHost h)) (Just ki@(KeterNodeId i)) f = case (T.stripSuffix h) =<< (T.stripSuffix (show i)  f) of
                                                                                                  (Just rest)
                                                                                                      |e == rest -> Right (ke,kh,ki)
                                                                                                      |otherwise -> Left .  KeterNodeError $ "exec doesn't match" 
                                                                                                  Nothing -> Left .  KeterNodeError $ "host or id doesn't match"
knUnSuffixFileName Nothing (Just kh@(KeterNodeHost h))  (Just ki@(KeterNodeId i)) f = case (T.stripSuffix h) =<< (T.stripSuffix (show i) f) of
                                                                                                      (Just rest) -> Right ((KeterNodeExec rest),kh,ki)
                                                                                                      Nothing -> Left .  KeterNodeError $ "host or id doesn't match"


knUnSuffixFileName (Just ke@(KeterNodeExec e)) Nothing  (Just ki@(KeterNodeId i)) f = case (T.stripPrefix e) =<< (T.stripSuffix (show i) f) of
                                                                                                           (Just rest) -> Right (ke,KeterNodeHost rest ,ki)
                                                                                                           Nothing -> Left .  KeterNodeError $ "host or id doesn't match"

knUnSuffixFileName (Just ke@(KeterNodeExec e)) (Just kh@(KeterNodeHost h))  Nothing  f =  case (T.stripPrefix e f) >>= (T.stripPrefix h)  of 
                                                                                                           (Just rest) -> (\r -> (ke,kh,KeterNodeId r)) <$> (rFcn rest)
                                                                                                           Nothing -> Left .  KeterNodeError $ "host or id doesn't match"
    where
      rFcn :: Text -> Either KeterNodeError Int 
      rFcn t = case (T.decimal  t) of
                  (Right (i,_)) -> Right . fromInteger $ i
                  Left e' -> Left . KeterNodeError . T.pack $ e'
                  

knUnSuffixFileName a b c  _ = Left . KeterNodeError $ "insufficient parameters provided, must have 2 of 3 to create triple" <> show a <> show b <> show c





-- | Change the name of the binary so it is unique

rewriteNodeBinary :: FilePath
                           -> KeterNodeStanzaConfig -> Text -> IO (Either Text FilePath)
rewriteNodeBinary wd knf fnew = do
--  print ("incoming filename"::Text) >> print fnew
  let
      kold = fromText.getKNExec.knsCfgExec $ knf 
      knew = fromText fnew
      oldDir = directory kold
      newDir = directory knew
      newFname = filename knew
  
  case oldDir == newDir of 
    False -> do 
        isFile kold >>= (\b -> unless b (print ("File Failure oldFile -->"::Text) >> print kold))
        isFile knew >>= (\b -> unless b (print ("File Failure newFile -->"::Text) >> print knew))
        unless (oldDir == newDir ) (print ("Dirs don't match"::Text) >> print oldDir >> print newDir)  
        fail "Error, invalid keter transformation"  
    True -> do 
      let kxform = oldDir </> newFname --transformed keter binary name 
      Sh.shelly $ (runCopyTransaction wd kold kxform) 


-- | Copy the exec file name appended with an appended host
runCopyTransaction :: FilePath
                            -> FilePath -> FilePath -> Sh.Sh (Either Text FilePath)
runCopyTransaction wd kold kxform = do 
--  wd'   <- liftIO $ getWorkingDirectory 
--  liftIO $  print "copy kold --> " >> print (wd' </> kold) >> print "to kxform --> " >> print (collapse (wd' </> kxform))
--  liftIO $  print "working directory" >> print wd'>> print "other idr" >> print wd
  Sh.cp (collapse kold) (collapse kxform)
--  liftIO $ print ("deleting" >> print kold
  Sh.rm kold
  let newKeterFile = ((filename kxform) <.> "keter")
  liftIO $ print ("making new tar"::Text) >> print newKeterFile
  enf <- traverse (\f -> Sh.run "tar" ["-czvf",f,"config","dist"])  (toText newKeterFile)
  liftIO $ print ("copying keter to correct dir --> " ::Text)>> print (wd </> activeNodes)
  Sh.cp newKeterFile (wd </> activeNodes)
  liftIO $ print ("leaving" :: Text)
  return $ enf >>= (\_-> Right newKeterFile) -- put the new filename in if everything went right  
  

