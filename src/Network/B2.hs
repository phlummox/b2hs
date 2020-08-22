
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.B2
  (
  -- * datatypes

    Credentials(..)
  , AuthInfo(..)
  , ConnectInfo(..)
  , UploadInfo(..)

  -- * operations

  , authorizeAccount
  , downloadFileByName
  , getUploadInfo
  , uploadFile

  )
  where

import            Crypto.Hash.SHA1 (hashlazy)

import            Data.Aeson
import qualified  Data.ByteString.Base16 as B16
import qualified  Data.ByteString.Char8 as BS
import qualified  Data.ByteString.Lazy as BSL
import            Data.Text (Text)

import            Lens.Micro.Extras as Ex
import            Lens.Micro.Platform hiding ( (.=) )
import            Network.HTTP.Types (urlEncode) --, urlDecode)
import            Network.Wreq as W hiding (Proxy)

import Network.B2.Internal
import Network.B2.Types

{-# ANN module ("HLint: ignore Use camelCase" ::String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" ::String) #-}

default(Text)


-- | authorize with the b2 service, get authorization
-- info.
authorizeAccount :: Credentials -> IO ConnectInfo
authorizeAccount creds =
  ConnectInfo creds . Ex.view W.responseBody <$>
                    (asJSON =<< makeAuthReq creds "b2_authorize_account")

-- |
-- @
-- downloadFileByName connInfo bucketName filePath
-- @
--
-- Download @filePath@ from @bucketName@ using @connInfo@.
--
-- Example:
--
-- > let creds = Credentials "SOME_APP_KEY_ID" "SOME_APP_KEY"
-- > conn <- authorizeAccount creds
-- > downloadFileByName conn "myBucket" "myfile.docx"
downloadFileByName ::
  ConnectInfo -> String -> String -> IO BSL.ByteString
downloadFileByName connInfo bucketName filePath =
  Ex.view W.responseBody <$> makeDownloadReq connInfo url
  where
    url = "file/" <> bucketName <> "/" <> filePath

-- | get upload info (an upload URL and auth token)
-- to upload a file to a bucket, specified by bucket ID.
getUploadInfo :: ConnectInfo -> String -> IO UploadInfo
getUploadInfo connInfo bucketId =
  Ex.view W.responseBody <$>
      (asJSON =<< makeApiPost connInfo "b2_get_upload_url" payload)
  where
    payload = object [
                  "bucketId" .= bucketId
                  ]

-- |
-- @
-- uploadFile connInfo bucketId fileName fileConts mimeType
-- @
--
-- Upload a file with name @fileName@, contents @fileConts@
-- and mime-type @mimeType@ to bucket @bucketId@.
--
-- Some useful mime types:
--
-- - text/plain
-- - application/octet-stream (binary types w/ no other type)
uploadFile :: ConnectInfo -> String -> FilePath -> BSL.ByteString -> String
  -> IO BSL.ByteString
uploadFile connInfo bucketId fileName fileConts mimeType = do
  upInfo <- getUploadInfo connInfo bucketId
  let upUrl     = unB2URL $ uploadUrl upInfo
      authTok   = authorizationToken (upInfo :: UploadInfo)
      sha1      = B16.encode $ hashlazy fileConts
      fileName' = urlEncode False $ BS.pack fileName
      opts      = defaults
                    & W.header "Authorization"     .~ [BS.pack authTok]
                    & W.header "X-Bz-File-Name"    .~ [fileName']
                    & W.header "Content-Type"      .~ [BS.pack mimeType]
                    & W.header "X-Bz-Content-Sha1" .~ [sha1]
  -- Content-Length is required, can't chunk.
  Ex.view W.responseBody <$> postWith opts upUrl fileConts


