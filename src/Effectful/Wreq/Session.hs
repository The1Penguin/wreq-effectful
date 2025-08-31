module Effectful.Wreq.Session
  (
  -- * Session creation
  Session
  , newSession
  , newAPISession

  -- ** More control-oriented session creation
  , newSessionControl

  -- * Get information about session state
  , getSessionCookieJar

  -- * HTTP verbs
  , get
  , post
  , head_
  , options
  , put
  , delete
  , customMethod

  -- * Configurable verbs
  , getWith
  , postWith
  , headWith
  , optionsWith
  , putWith
  , deleteWith
  , customMethodWith
  , customPayloadMethodWith
  , customHistoriedMethodWith
  , customHistoriedPayloadMethodWith
  )
    where

import Data.ByteString.Lazy                 (ByteString)
import Effectful
import Effectful.Dispatch.Static
import Effectful.Wreq                       (Wreq)
import Network.HTTP.Client
import Network.Wreq.Session                 (Session)
import Network.Wreq.Session qualified as S
import           Network.Wreq.Types

-- | Lifted `S.newSession`
newSession :: Wreq :> es => Eff es Session
newSession = unsafeEff_ S.newSession

-- | Lifted `S.newAPISession`
newAPISession :: Wreq :> es => Eff es Session
newAPISession = unsafeEff_ S.newAPISession

-- | Lifted `S.newSessionControl`
newSessionControl :: Wreq :> es => Maybe CookieJar -> ManagerSettings -> Eff es Session
newSessionControl cj = unsafeEff_ . S.newSessionControl cj

-- | Lifted `S.getSessionCookieJar`
getSessionCookieJar :: Wreq :> es => Session -> Eff es (Maybe CookieJar)
getSessionCookieJar = unsafeEff_ . S.getSessionCookieJar

-- | Lifted `S.get`
get :: Wreq :> es => Session -> String -> Eff es (Response ByteString)
get session = unsafeEff_ . S.get session

-- | Lifted `S.post`
post :: (Wreq :> es, Postable a) => Session -> String -> a -> Eff es (Response ByteString)
post session url = unsafeEff_ . S.post session url

-- | Lifted `S.head_`
head_ :: Wreq :> es => Session -> String -> Eff es (Response ())
head_ session = unsafeEff_ . S.head_ session

-- | Lifted `S.options`
options :: Wreq :> es => Session -> String -> Eff es (Response ())
options session = unsafeEff_ . S.options session

-- | Lifted `S.put`
put :: (Wreq :> es, Putable a) => Session -> String -> a -> Eff es (Response ByteString)
put session url = unsafeEff_ . S.put session url

-- | Lifted `S.delete`
delete :: Wreq :> es => Session -> String -> Eff es (Response ByteString)
delete session = unsafeEff_ . S.delete session

-- | Lifted `S.customMethod`
customMethod :: Wreq :> es => String -> Session -> String -> Eff es (Response ByteString)
customMethod method session = unsafeEff_ . S.customMethod method session

-- | Lifted `S.getWith`
getWith :: Wreq :> es => Options -> Session -> String -> Eff es (Response ByteString)
getWith opts session = unsafeEff_ . S.getWith opts session

-- | Lifted `S.postWith`
postWith :: (Wreq :> es, Postable a) => Options -> Session -> String -> a -> Eff es (Response ByteString)
postWith opts session url = unsafeEff_ . S.postWith opts session url

-- | Lifted `S.headWith`
headWith :: Wreq :> es => Options -> Session -> String -> Eff es (Response ())
headWith opts session = unsafeEff_ . S.headWith opts session

-- | Lifted `S.optionsWith`
optionsWith :: Wreq :> es => Options -> Session -> String -> Eff es (Response ())
optionsWith opts session = unsafeEff_ . S.optionsWith opts session

-- | Lifted `S.putWith`
putWith :: (Wreq :> es, Putable a) => Options -> Session -> String -> a -> Eff es (Response ByteString)
putWith opts session url = unsafeEff_ . S.putWith opts session url

-- | Lifted `S.deleteWith`
deleteWith :: Wreq :> es => Options -> Session -> String -> Eff es (Response ByteString)
deleteWith opts session = unsafeEff_ . S.deleteWith opts session

-- | Lifted `S.customMethodWith`
customMethodWith :: Wreq :> es => String -> Options -> Session -> String -> Eff es (Response ByteString)
customMethodWith method opts session = unsafeEff_ . S.customMethodWith method opts session

-- | Lifted `S.customPayloadMethodWith`
customPayloadMethodWith :: (Wreq :> es, Postable a) => String -> Options -> Session -> String -> a -> Eff es (Response ByteString)
customPayloadMethodWith method opts session url = unsafeEff_ . S.customPayloadMethodWith method opts session url

-- | Lifted `S.customHistoriedMethodWith`
customHistoriedMethodWith :: Wreq :> es => String -> Options -> Session -> String -> Eff es (HistoriedResponse ByteString)
customHistoriedMethodWith method opts session = unsafeEff_ . S.customHistoriedMethodWith method opts session

-- | Lifted `S.customHistoriedPayloadMethodWith`
customHistoriedPayloadMethodWith :: (Wreq :> es, Postable a) => String -> Options -> Session -> String -> a -> Eff es (HistoriedResponse ByteString)
customHistoriedPayloadMethodWith method opts session url = unsafeEff_ . S.customHistoriedPayloadMethodWith method opts session url
