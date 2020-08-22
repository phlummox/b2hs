
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{- |

Low-level functions

-}

module Network.B2.Internal
  (
    authOpts
  , makeAuthReq
  , makeDownloadReq
  , makeApiGet
  , makeApiPost
  )
  where

import            Data.Aeson (Value)
import qualified  Data.ByteString.Lazy as BSL
import            Data.Text.Encoding (encodeUtf8)
import            Lens.Micro.Platform hiding ( (.=) )
import            Network.Wreq as W hiding (Proxy)

import Network.B2.Internal.Types
import Network.B2.Types

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
-- make an API GET request. @urlSuffix@ is everything after
-- the \"@https://hostname/b2api/v2/@" bit - basically a single
-- "command" (or component in a URL path).
--
-- "API" requests are basically everything that isn't
-- an authentication, upload or download request.
makeApiGet ::
  ConnectInfo -> String -> IO (Response BSL.ByteString)
makeApiGet connInfo urlSuffix = do
  let (B2URL apiUrl')  = apiUrl $ authInfo connInfo
      url       = apiUrl' <> "/b2api/v2/" <> urlSuffix
  getWith (authOpts connInfo)  url


-- |
-- @
-- makeApiPost connInfo urlSuffix payload
-- @
--
-- make an API POST request. @urlSuffix@ is everything after
-- the \"@https://hostname/b2api/v2/@" bit - basically a single
-- "command" (or component in a URL path).
--
-- "API" requests are basically everything that isn't
-- an authentication, upload or download request.
makeApiPost ::
  ConnectInfo -> String -> Value -> IO (Response BSL.ByteString)
makeApiPost connInfo urlSuffix payload = do
  let (B2URL apiUrl')  = apiUrl $ authInfo connInfo
      url       =  apiUrl' <> "/b2api/v2/" <> urlSuffix
  postWith (authOpts connInfo)  url payload


