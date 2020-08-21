
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Network.B2
  where

import            Crypto.Hash.SHA1 (hashlazy)

import            Data.Aeson
import qualified  Data.ByteString.Base16 as B16
import qualified  Data.ByteString.Char8 as BS
import qualified  Data.ByteString.Lazy as BSL
import            Data.Int
import qualified  Data.Map as M
import            Data.Proxy
import            Data.String
import qualified  Data.Text as T
import            Data.Text.Encoding (encodeUtf8)
import            Data.Text as T

import            GHC.Generics
import            Lens.Micro.Extras as Ex
import            Lens.Micro.Platform hiding ( (.=) )
import            Network.HTTP.Types (urlEncode) --, urlDecode)
import            Network.Wreq as W hiding (Proxy)
--import            System.FilePath ( takeFileName )

{-# ANN module ("HLint: ignore Use camelCase" ::String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" ::String) #-}

default(Text)

-- --
-- for use as phantom types.
--
-- The b2 api expects us to make requests to different hostnames
-- depending on whether we're getting authorization tokens/info;
-- downloading a file; or making some other API call.
--
-- So we distinguish them.

data AUTH
data DOWNLOAD
data API
data UPLOAD

class URLType t where
  showTag :: Proxy t -> String

instance URLType AUTH where
  showTag _ = "AUTH"

instance URLType DOWNLOAD where
  showTag _ = "DOWNLOAD"

instance URLType API where
  showTag _ = "API"

instance URLType UPLOAD where
  showTag _ = "UPLOAD"

-- | B2URLs.
--
-- Expected phantom types are: 'AUTH' (URLs for getting
-- an AuthInfo object are labelled with this),
-- 'DOWNLOAD' (for downloading a file) and 'API'
-- (for making other API requests).
newtype B2URL a = B2URL { unB2URL :: String }
  deriving (Eq, IsString)

instance URLType a => Show (B2URL a) where
  show = showURL
    where
      showURL :: forall a . URLType a => B2URL a -> String
      showURL (B2URL u) = showTag p <> " " <> show u
          where
            p :: Proxy a
            p = Proxy

-- --
-- constants

-- | basically a hostname (plus scheme) -
--  used for initial authentication request
pattern B2V2_AUTH_URL :: B2URL AUTH
pattern B2V2_AUTH_URL = B2URL "https://api.backblazeb2.com"

-- --
-- datatypes

data Credentials =
    Credentials
    {
      applicationKeyId  :: Text
    , applicationKey    :: Text
    }
    deriving Eq

instance Show Credentials where
  show (Credentials keyId _) = "Credentials " <> T.unpack keyId
                                <> " <KEY>"

-- | Info got from a call to @b2_authorize_account@ endpoint
data AuthInfo = AuthInfo
    { accountId           :: Text
    , authorizationToken  :: Text
    , allowed             :: M.Map Text Value
    , apiUrl              :: B2URL API -- ^ "api-host-bit" might be a better
                                       --   description.
    , downloadUrl         :: B2URL DOWNLOAD
    , recommendedPartSize :: Int64
    } deriving (Eq, Show, Generic)

instance FromJSON (B2URL DOWNLOAD) where
  parseJSON v = B2URL <$> parseJSON v

instance ToJSON (B2URL DOWNLOAD) where
  toJSON (B2URL url) = toJSON url

instance FromJSON (B2URL API) where
  parseJSON v = B2URL <$> parseJSON v

instance ToJSON (B2URL API) where
  toJSON (B2URL url) = toJSON url

instance FromJSON (B2URL UPLOAD) where
  parseJSON v = B2URL <$> parseJSON v

instance ToJSON (B2URL UPLOAD) where
  toJSON (B2URL url) = toJSON url

instance FromJSON AuthInfo where
instance ToJSON AuthInfo where

-- | info to required to connect for most endpoint calls
data ConnectInfo =
  ConnectInfo {
      credentials :: Credentials
    , authInfo    :: AuthInfo
    }
    deriving (Eq, Show)

-- | Sent in response to b2_get_upload_url
data UploadInfo =
  UploadInfo {
      authorizationToken :: String
    , bucketId           :: Text
    , uploadUrl          :: B2URL UPLOAD
    }
    deriving (Eq, Show, Generic)

instance FromJSON UploadInfo where
instance ToJSON   UploadInfo where

-- ---
-- low-level funcs

-- | Wreq request options: add "Authorization" to headers
authOpts :: ConnectInfo -> W.Options
authOpts connInfo =
  let
      authTok = encodeUtf8 $ authorizationToken (authInfo connInfo :: AuthInfo)
  in  defaults & W.header "Authorization" .~ [authTok]

-- |
-- @
-- makeAuthReq connInfo urlSuffix
-- @
--
-- make an authentication request. @urlSuffix@ is everything after
-- the \"@v2/@". (really the only option is \"@b2_authorize_account@".
makeAuthReq ::
  Credentials -> String -> IO (Response BSL.ByteString)
makeAuthReq (Credentials aId appKey) urlSuffix =
    getWith opts url
  where
    url = unB2URL B2V2_AUTH_URL <> "/b2api/v2/" <> urlSuffix
    opts = defaults & auth ?~ W.basicAuth (encodeUtf8 aId) (encodeUtf8 appKey)


-- |
-- @
-- makeDownloadReq connInfo urlSuffix
-- @
--
-- make a download request. @urlSuffix@ is everything after
-- the \"@https://hostname/@" bit.
makeDownloadReq ::
  ConnectInfo -> String -> IO (Response BSL.ByteString)
makeDownloadReq connInfo urlSuffix = do
  let (B2URL dlUrl)  = downloadUrl $ authInfo connInfo
      url       = dlUrl <> "/" <> urlSuffix
  getWith (authOpts connInfo) url

-- |
-- @
-- makeApiGet connInfo urlSuffix
-- @
--
-- make an API request. @urlSuffix@ is everything after
-- the \"@https://hostname/b2api/v2/@" bit - basically a single
-- "command" (or component in a URL path).
makeApiGet ::
  ConnectInfo -> String -> IO (Response BSL.ByteString)
makeApiGet connInfo urlSuffix = do
  let (unB2URL -> apiUrl')  = apiUrl $ authInfo connInfo
      url       = apiUrl' <> "/b2api/v2/" <> urlSuffix
  getWith (authOpts connInfo)  url

makeApiPost ::
  ConnectInfo -> String -> Value -> IO (Response BSL.ByteString)
makeApiPost connInfo urlSuffix payload = do
  let (unB2URL -> apiUrl')  = apiUrl $ authInfo connInfo
      url       =  apiUrl' <> "/b2api/v2/" <> urlSuffix
  postWith (authOpts connInfo)  url payload

-- ---
-- higher level

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
getUploadInfo :: ConnectInfo -> Text -> IO UploadInfo
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
uploadFile :: ConnectInfo -> Text -> FilePath -> BSL.ByteString -> String
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


