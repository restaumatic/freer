{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Union where

import Data.Open.Union

import GHC.TypeLits
import Data.Proxy

import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
                           -- Union Tests --
--------------------------------------------------------------------------------

data Sing (n :: Nat) a = Sing Word

checkValid :: forall n a. KnownNat n => Maybe (Sing n a) -> Assertion
checkValid (Just (Sing n)) = if n == reify @n then pure () else error ("invalid item returned: expected " <> show (reify @n) <> ", got " <> show n)
checkValid Nothing = error "projection failed"

sing :: forall n a. KnownNat n => Sing n a
sing = Sing (reify @n)

reify :: forall n. KnownNat n => Word
reify = fromIntegral (natVal (Proxy @n))

type U = '[Sing 0, Sing 1, Sing 2, Sing 3]

unionTests :: TestTree
unionTests =
  testGroup "Union tests"
    [ testCase "weakenAt - union elem is before index" $
        checkValid $ prj @(Sing 1) $ weakenAt @2 @(Sing 5) $ inj @_ @U (sing @1)
    , testCase "weakenAt - union elem is at index" $
        checkValid $ prj @(Sing 2) $ weakenAt @2 @(Sing 5) $ inj @_ @U (sing @2)
    , testCase "weakenAt - union elem is after index" $
        checkValid $ prj @(Sing 3) $ weakenAt @2 @(Sing 5) $ inj @_ @U (sing @3)
    ]
