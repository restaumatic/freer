{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}

module Eff.InterpreterHelpers
  ( runWith
  , runWithS
  , interceptWith
  , tapWith
  ) where

import           Eff          (Eff, Member, handleRelay, handleRelayS, send)
import           Eff.Internal (interpose)

-- | Interpret one effect in terms of the remaining effects.
runWith :: (forall a. f a -> Eff effs a) -> Eff (f ': effs) b -> Eff effs b
runWith interpret = handleRelay pure (\k q -> interpret k >>= q)

-- | Interpret one effect in terms of the remaining effects.
runWithS :: s -> (forall a. s -> f a -> Eff effs (s, a)) -> Eff (f ': effs) b -> Eff effs b
runWithS initState interpret = handleRelayS initState (const pure) (\s k q -> do (s', r) <- interpret s k; q s' r)

-- | Interpret one effect but do not remove it from stack.
interceptWith :: Member f effs => (forall a. f a -> Eff effs a) -> Eff effs b -> Eff effs b
interceptWith interpret = interpose pure (\k q -> interpret k >>= q)

-- | Silently interpret one effect but do not remove it from stack.
tapWith :: Member f effs => (forall a. f a -> Eff effs ()) -> Eff effs b -> Eff effs b
tapWith interpret = interpose pure (\k q -> interpret k >> send k >>= q)
