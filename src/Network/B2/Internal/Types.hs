

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Network.B2.Internal.Types
  (
  -- * tags

  -- $tags
    AUTH
  , DOWNLOAD
  , API
  , UPLOAD

  -- ** tag class

  , URLType(..)

  -- TODO: HIDE INTERNALS:

  , B2URL(..)

  )
  where

import            Data.Aeson
import            Data.Proxy
import            Data.String

-- $tags
--
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


