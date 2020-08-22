
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.B2.Types
  (
  -- * extract conts of internal url type

    unB2URL

  -- * constants

  , pattern B2V2_AUTH_URL

  -- * datatypes

  , Credentials(..)
  , AuthInfo(..)
  , ConnectInfo(..)
  , UploadInfo(..)
  )
  where

import            Data.Aeson
import            Data.Int (Int64)
import qualified  Data.Map as M
import qualified  Data.Text as T
import            Data.Text (Text)

import            GHC.Generics


import Network.B2.Internal.Types



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

instance FromJSON AuthInfo where
instance ToJSON AuthInfo where


-- | info to required to connect for most endpoint calls
data ConnectInfo =
  ConnectInfo {
      credentials :: Credentials
    , authInfo    :: AuthInfo
    }
    deriving (Eq, Show)


-- | Sent from b2 server in response to b2_get_upload_url
data UploadInfo =
  UploadInfo {
      authorizationToken :: String
    , bucketId           :: Text
    , uploadUrl          :: B2URL UPLOAD
    }
    deriving (Eq, Show, Generic)

instance FromJSON UploadInfo where
instance ToJSON   UploadInfo where



