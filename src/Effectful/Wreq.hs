module Effectful.Wreq (
    Wreq
  , runWreq
  ) where

import Effectful
import Effectful.Dispatch.Static

data Wreq :: Effect where

type instance DispatchOf Wreq = Static WithSideEffects
data instance StaticRep Wreq = Wreq

-- | Run the 'Wreq' effect.
runWreq :: (HasCallStack, IOE :> es) => Eff (Wreq : es) a -> Eff es a
runWreq = evalStaticRep Wreq
