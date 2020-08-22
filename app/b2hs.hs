
{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import qualified Data.ByteString.Lazy.Char8 as BSL

import System.FilePath as FP

import Network.B2
import ProgOptions

cmdHandler :: Credentials -> Command -> IO ()
cmdHandler creds cmd = do
  conn <- authorizeAccount creds
  case cmd of
    DownloadFileByName{} ->
        downloadFileByName conn (dlBucketName cmd) (dlFilePath cmd) >>=
            BSL.writeFile (dlTarget cmd)
    UploadFile{} -> do
        bs  <- BSL.readFile (ulFilePath cmd)
        let remoteName = FP.takeFileName (ulFilePath cmd)
        res <- uploadFile conn (ulBucketId cmd) remoteName bs (ulMimeType cmd)
        BSL.putStrLn $ "upload result:\n\n" <> res


main :: IO ()
main = processArgs cmdHandler


