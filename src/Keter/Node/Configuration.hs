{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Keter.Node.Configuration ( buildKeterNode, decodeKeterNodeStanzaConfig
                                ) where

-- Control Libraries 
import BasicPrelude 
import Data.Traversable
import Data.Default
import Control.Monad
import qualified Control.Lens as L 

-- System Libraries 
import Filesystem (getWorkingDirectory,isFile,setWorkingDirectory)
import Filesystem.Path.CurrentOS (directory,fromText,toText, encodeString, (<.>),(</>),empty,filename,directory,collapse)
import qualified Codec.Archive.TempTarball as TempFolder
import qualified Shelly as Sh

-- MonoMorphs
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- Parsing
import Data.Yaml
import Data.Yaml.FilePath
import Data.Text.Template


-- Containers 
import qualified Data.Map.Strict as M
import qualified Data.Set as S


-- Project specific
import Keter.App
import Keter.AppManager
import Keter.Types


-- Local Libraries
import Keter.Node.Internal
import Keter.Node.Types
import Keter.Node.Tests
import Keter.Node.LensComb


decodeKeterNodeStanzaConfig :: FilePath -> IO ( Either ParseException KeterNodeStanzaConfig)
decodeKeterNodeStanzaConfig = decodeFileRelative   

decodeKeterStanzaConfig :: FilePath -> IO (Either ParseException BundleConfig) 
decodeKeterStanzaConfig = decodeFileRelative

-- | buildKeterNode parses the keter-node.yaml file and uses it to modify the keter.yaml file 
-- put the file in the config directory, right next to the keter.yaml file 

 -- | Create 'Context' from association list.

buildKeterNode fp = do
  wd <- getWorkingDirectory
  let newWd = wd </> fp
      config = newWd </> "config" 
      knfilePth = config </> "keter-node" <.> "yaml"
      kfilePth = config </> "keter" <.> "yaml"
  putStrLn "Done Getting Directories"
  eKNSC <- decodeKeterNodeStanzaConfig knfilePth  
  print kfilePth
  setWorkingDirectory newWd 
  putStrLn "Done decoding node"  
  eKSC <- decodeKeterStanzaConfig kfilePth
  putStrLn "Done decoding keter"
  case eKNSC of 
    Left e -> print e 
    Right knf -> do 
               print "before append " >> (print $ getKNExec . knsCfgExec $ knf)
               let h = S.findMin . knsCfgHosts $ knf 
                   p = S.findMin . knsCfgPorts $ knf 
                   e = knsCfgExec $ knf 
                   h' = getKNHost $ h -- convert everything to text 
                   p' = show.getKNPort $ p
                   e' = T.append (T.append (getKNExec e) p') h' --make new executable nameg
                   mp = M.fromList [("host",h' )  -- Build the lookup map
                                   ,("port" ,p')
                                   ,("exec" , e')] 
                   context :: ContextA Maybe
                   context t = M.lookup t mp
               kfile <- readFile kfilePth
               putStrLn "To the substituteA case"
               case substituteA kfile context of 
                 Nothing -> error "No File Read or invalid template var" 
                 Just txt -> do                      
                      writeFile kfilePth (TL.toStrict txt)
                      putStrLn "Done writing files"
                      finally (rewriteNodeBinary wd knf e' >> putStrLn "Done rewriting exec") (setWorkingDirectory wd)


-- | Change the name of the binary so it is unique
rewriteNodeBinary :: FilePath -> KeterNodeStanzaConfig -> T.Text-> IO () 
rewriteNodeBinary wd knf fnew = do
  print "incoming filename" >> print fnew
  let
      kold = fromText.getKNExec.knsCfgExec $ knf 
      knew = fromText fnew
      oldDir = directory kold
      newDir = directory knew
      newFname = filename knew
  
  case oldDir == newDir of 
    False -> do 
        isFile kold >>= (\b -> unless b (print "File Failure oldFile -->" >> print kold))
        isFile knew >>= (\b -> unless b (print "File Failure newFile -->" >> print knew))
        unless (oldDir == newDir ) (print "Dirs don't match" >> print oldDir >> print newDir)  
        fail "Error, invalid keter transformation"  
    True -> do 
      let kxform = oldDir </> newFname --transformed keter binary name 
      Sh.shelly $ runCopyTransaction wd kold kxform
      return ()      


-- | Copy the exec file name appended with an appended host
runCopyTransaction wd kold kxform = do 
  kold' <- Sh.absPath kold
  kxform' <- Sh.absPath kxform
  liftIO $  print "copy kold --> " >> print (kold') >> print "to kxform --> " >> print (kxform')
  liftIO $  print "working directory" >> getWorkingDirectory >>= print >> print "other idr" >> print wd
  Sh.cp kold' (collapse kxform')
  Sh.rm kold'
  let newKeterFile = ((filename kxform') <.> "keter")
  traverse (\f -> Sh.run "tar" ["-czvf",f,"config","dist"])  (toText newKeterFile)
  Sh.cp newKeterFile (wd </> activeNodes)
  

