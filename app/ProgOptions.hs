
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-partial-fields #-}


module ProgOptions
  --(
  --  module ProgOptions
  --, loadFromConfigFile
  --)
  where

import qualified  Data.List as L
import            Data.Semigroup ((<>))
import qualified  Data.Text as T

import            Options.Applicative as Op
import            System.Environment
import            System.Exit
import            System.IO
import            Text.PrettyPrint.ANSI.Leijen hiding ( (<$>), (<>) )


import Version
import Network.B2.Types

default(String)

data Command =
    DownloadFileByName  { dlBucketName :: String
                        , dlFilePath   :: FilePath
                        , dlTarget     :: FilePath
                        }
  | UploadFile          { ulBucketId   :: String
                        , ulFilePath   :: FilePath
                        , ulMimeType   :: String
                        }
  deriving (Eq, Show)


credentialsDoc :: Doc
credentialsDoc =
  text ("The environment variables H2BS_APPLICATION_KEY_ID"
      ++  " and H2BS_APPLICATION_KEY should be set to pass in"
      ++  " credentials."
  )

versionOption :: Parser (a -> a)
versionOption =
  infoOption dumpVersion ( long "version"
                        <> short 'v'
                        <> help "display version"
                        )

numericVersionOption :: Parser (a -> a)
numericVersionOption =
  infoOption (version getVersion)
                        ( long "numeric-version"
                        <> help "display numeric version"
                        )

versionDoc :: Doc
versionDoc =
  mconcat $ L.intersperse line $ map text $ lines dumpVersion


dlOpts :: Parser Command
dlOpts =
  DownloadFileByName
        <$> argument str  (metavar "BUCKET_NAME"
                          <> help "bucket to download from"
                          )
        <*> argument str  (metavar "FILE_PATH"
                            <> help "file to download"
                          )
        <*> argument str  (metavar "TARGET_PATH"
                            <> help ("where to  download file to"
                                     <> " (default stdout)")
                            <> value "/dev/stdout"
                          )

ulOpts :: Parser Command
ulOpts =
  UploadFile
        <$> argument str  (metavar "BUCKET_ID"
                          <> help "bucket ID to upload to"
                          )
        <*> argument str  (metavar "FILE_PATH"
                            <> help "file to upload"
                          )
        <*> argument str  (metavar "MIME_TYPE"
                            <> help "file mime-type"
                            <> value "application/octet-stream"
                          )

commandParse :: Parser Command
commandParse = subparser
          (
          command "download"
              (info (dlOpts <**> helper)
                    (progDesc "download a file"
                    <>
                    footerDoc (Just credentialsDoc)
                    ))
          <>
          command "upload"
              (info (ulOpts <**> helper)
                    (progDesc "upload a file"
                    <>
                    footerDoc (Just credentialsDoc)
                    ))

          )

-- | add program info, helper, @--version@ options to
-- our opts.
opts :: ParserInfo Command
opts = info
          (commandParse
              <**> helper
              <**> versionOption
              <**> numericVersionOption)
          ( fullDesc
              <> progDesc "backblaze b2 up/download"
              <> headerDoc (Just $
                    text "b2hs - backblaze b2 up/downloader"
                    <> line <> line
                    <> versionDoc
                    <> line <> line
                    <> credentialsDoc))

-- | get credentials from env vars
getCredentials :: IO Credentials
getCredentials = do
  appKeyId   <- lookupEnv "H2BS_APPLICATION_KEY_ID" >>= \case
                  Nothing -> do hPutStrLn stderr $ "env var "
                                  <> "H2BS_APPLICATION_KEY_ID is not set"
                                exitFailure
                  Just j  -> return j
  appKey     <- lookupEnv "H2BS_APPLICATION_KEY" >>= \case
                  Nothing -> do hPutStrLn stderr $ "env var "
                                  <> "H2BS_APPLICATION_KEY is not set"
                                exitFailure
                  Just j  -> return j

  return $ Credentials (T.pack appKeyId) (T.pack appKey)


-- | parse the args, get creds, and call
-- @cmdHandler@ callback to deal with them.
processArgs ::
  (Credentials -> Command -> IO b) -> IO b
processArgs cmdHandler = do
  cmd <- execParser opts
  creds <- getCredentials
  cmdHandler creds cmd


