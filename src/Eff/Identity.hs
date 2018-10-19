{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Eff.Identity where

import           Prelude (error, pure)
import           Eff     (Eff, handleRelay)

data Identity e v

runIdentity :: Eff (Identity e ': effs) w -> Eff effs w
runIdentity = handleRelay pure (error "Impossible: Identity effect was called")
