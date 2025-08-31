module Effectful.Wreq (
  -- * Effect
    Wreq
  -- ** Handlers
  , runWreq

  -- * GET
  , get
  , getWith

  -- * POST
  , post
  , postWith

  -- * HEAD
  , head_
  , headWith

  -- * PUT
  , put
  , putWith

  -- * PATCH
  , patch
  , patchWith

  -- * OPTIONS
  , options
  , optionsWith

  -- * DELETE
  , delete
  , deleteWith

  -- * Custom Method
  , customMethod
  , customMethodWith
  , customHistoriedMethod
  , customHistoriedMethodWith

  -- * Custom Payload Method
  , customPayloadMethod
  , customPayloadMethodWith
  , customHistoriedPayloadMethod
  , customHistoriedPayloadMethodWith

  -- * Reexports
  , W.Options
  , W.defaults
  , WL.manager
  , WL.header
  , WL.param
  , WL.redirects
  , WL.headers
  , WL.params
  , WL.cookie
  , WL.cookies
  , WL.checkResponse

  -- ** Authentication
  , W.Auth
  , W.AWSAuthVersion(..)
  , WL.auth
  , W.basicAuth
  , W.oauth1Auth
  , W.oauth2Bearer
  , W.oauth2Token
  , W.awsAuth
  , W.awsFullAuth
  , W.awsSessionTokenAuth
  -- ** Proxy settings
  , W.Proxy(Proxy)
  , WL.proxy
  , W.httpProxy
  -- ** Using a manager with defaults
  , W.withManager

  -- ** Payloads for POST and PUT
  , W.Payload(..)
  -- *** URL-encoded form data
  , W.FormParam(..)
  , W.FormValue
  -- *** Multipart form data
  , HF.Part
  , WL.partName
  , WL.partFileName
  , WL.partContentType
  , WL.partGetBody
  -- **** Smart constructors
  , HF.partBS
  , HF.partLBS
  , W.partText
  , W.partString
  , HF.partFile
  , HF.partFileSource

  -- ** Responses
  , W.Response
  , WL.responseBody
  , WL.responseHeader
  , WL.responseLink
  , WL.responseCookie
  , WL.responseHeaders
  , WL.responseCookieJar
  , WL.responseStatus
  , WL.Status
  , WL.statusCode
  , WL.statusMessage
  , W.HistoriedResponse
  , WL.hrFinalRequest
  , WL.hrFinalResponse
  , WL.hrRedirects
  -- *** Link headers
  , WL.Link
  , WL.linkURL
  , WL.linkParams
  -- *** Decoding responses
  , W.JSONError(..)
  , W.asJSON
  , W.asValue

  -- ** Cookies
  , WL.Cookie
  , WL.cookieName
  , WL.cookieValue
  , WL.cookieExpiryTime
  , WL.cookieDomain
  , WL.cookiePath

  -- ** Parsing responses
  , WL.atto
  , WL.atto_
  ) where

import Effectful
import Effectful.Dispatch.Static
import Data.ByteString.Lazy qualified as L
import Network.HTTP.Client.MultipartFormData qualified as HF
import Network.Wreq         qualified as W
import Network.Wreq.Lens    qualified as WL
import Network.Wreq.Types as WT

data Wreq :: Effect where

type instance DispatchOf Wreq = Static WithSideEffects
data instance StaticRep Wreq = Wreq

-- | Run the 'Wreq' effect.
runWreq :: (HasCallStack, IOE :> es) => Eff (Wreq : es) a -> Eff es a
runWreq = evalStaticRep Wreq

-- | Lifted `W.get`
get :: Wreq :> es => String -> Eff es (W.Response L.ByteString)
get = unsafeEff_ . W.get

-- | Lifted `W.getWith`
getWith :: Wreq :> es => W.Options -> String -> Eff es (W.Response L.ByteString)
getWith opts = unsafeEff_ . W.getWith opts

-- | Lifted `W.post`
post :: Wreq :> es => WT.Postable a => String -> a -> Eff es (W.Response L.ByteString)
post url = unsafeEff_ . W.post url

-- | Lifted `W.postWith`
postWith :: Wreq :> es => WT.Postable a => W.Options -> String -> a -> Eff es (W.Response L.ByteString)
postWith opts url = unsafeEff_ . W.postWith opts url

-- | Lifted `W.head_`
head_ :: Wreq :> es => String -> Eff es (W.Response ())
head_ = unsafeEff_ . W.head_

-- | Lifted `W.headWith`
headWith :: Wreq :> es => W.Options -> String -> Eff es (W.Response ())
headWith opts = unsafeEff_ . W.headWith opts

-- | Lifted `W.put`
put :: Wreq :> es => Putable a => String -> a -> Eff es (W.Response L.ByteString)
put url = unsafeEff_ . W.put url

-- | Lifted `W.putWith`
putWith :: Wreq :> es => Putable a => W.Options -> String -> a -> Eff es (W.Response L.ByteString)
putWith opts url = unsafeEff_ . W.putWith opts url

-- | Lifted `W.patch`
patch :: Wreq :> es => Patchable a => String -> a -> Eff es (W.Response L.ByteString)
patch url = unsafeEff_ . W.patch url

-- | Lifted `W.patchWith`
patchWith :: Wreq :> es => Patchable a => W.Options -> String -> a -> Eff es (W.Response L.ByteString)
patchWith opts url = unsafeEff_ . W.patchWith opts url

-- | Lifted `W.options`
options :: Wreq :> es => String -> Eff es (W.Response ())
options = unsafeEff_ . W.options

-- | Lifted `W.optionsWith`
optionsWith :: Wreq :> es => W.Options -> String -> Eff es (W.Response ())
optionsWith opts = unsafeEff_ . W.optionsWith opts

-- | Lifted `W.delete`
delete :: Wreq :> es => String -> Eff es (W.Response L.ByteString)
delete = unsafeEff_ . W.delete

-- | Lifted `W.deleteWith`
deleteWith :: Wreq :> es => W.Options -> String -> Eff es (W.Response L.ByteString)
deleteWith opts = unsafeEff_ . W.deleteWith opts

-- | Lifted `W.customMethod`
customMethod :: Wreq :> es => String -> String -> Eff es (W.Response L.ByteString)
customMethod method = unsafeEff_ . W.customMethod method

-- | Lifted `W.customMethodWith`
customMethodWith :: Wreq :> es => String -> W.Options -> String -> Eff es (W.Response L.ByteString)
customMethodWith method opts = unsafeEff_ . W.customMethodWith method opts

-- | Lifted `W.customHistoriedMethod`
customHistoriedMethod :: Wreq :> es => String -> String -> Eff es (W.HistoriedResponse L.ByteString)
customHistoriedMethod method = unsafeEff_ . W.customHistoriedMethod method

-- | Lifted `W.customHistoriedMethodWith`
customHistoriedMethodWith :: Wreq :> es => String -> W.Options -> String -> Eff es (W.HistoriedResponse L.ByteString)
customHistoriedMethodWith method opts = unsafeEff_ . W.customHistoriedMethodWith method opts

-- | Lifted `W.customPayloadMethod`
customPayloadMethod :: Wreq :> es => WT.Postable a => String -> String -> a -> Eff es (W.Response L.ByteString)
customPayloadMethod method url = unsafeEff_ . W.customPayloadMethod method url

-- | Lifted `W.customPayloadMethodWith`
customPayloadMethodWith :: Wreq :> es => WT.Postable a => String -> W.Options -> String -> a -> Eff es (W.Response L.ByteString)
customPayloadMethodWith method opts url = unsafeEff_ . W.customPayloadMethodWith method opts url

-- | Lifted `W.customHistoriedPayloadMethod`
customHistoriedPayloadMethod :: Wreq :> es => WT.Postable a => String -> String -> a -> Eff es (W.HistoriedResponse L.ByteString)
customHistoriedPayloadMethod method url = unsafeEff_ . W.customHistoriedPayloadMethod method url

-- | Lifted `W.customHistoriedPayloadMethodWith`
customHistoriedPayloadMethodWith :: Wreq :> es => WT.Postable a => String -> W.Options -> String -> a -> Eff es (W.HistoriedResponse L.ByteString)
customHistoriedPayloadMethodWith method opts url = unsafeEff_ . W.customHistoriedPayloadMethodWith method opts url

