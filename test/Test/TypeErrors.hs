{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

 -- Deferring type errors is necessary for should-not-typecheck to work.
{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Test.TypeErrors where

import Data.Functor.Identity (Identity(Identity))
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Data.WorldPeace (Union(..), unionRemove)


unionRemoveTypeErrors :: TestTree
unionRemoveTypeErrors =
  testGroup
    "unionRemove should not typecheck"
    [ testCase "too few types in resulting union 1" $ do
        let u = This (Identity "hello") :: Union Identity '[String]
        shouldNotTypecheck
          (unionRemove u :: Either (Identity Double) (Union Identity '[]))
    , testCase "too few types in resulting union 2" $ do
        let u = This (Identity "hello") :: Union Identity '[String, Char, Double]
        shouldNotTypecheck
          (unionRemove u :: Either (Identity Double) (Union Identity '[String]))
    , testCase "too many types in resulting union 1" $ do
        let u = This (Identity "hello") :: Union Identity '[String]
        shouldNotTypecheck
          (unionRemove u :: Either (Identity Double) (Union Identity '[String, String]))
    , testCase "too many types in resulting union 2" $ do
        let u = This (Identity "hello") :: Union Identity '[String, Char, Double]
        shouldNotTypecheck
          (unionRemove u :: Either (Identity Double) (Union Identity '[String, Char, Double]))
    , testCase "does not pull out multiple" $ do
        let u = This (Identity "hello") :: Union Identity '[String, String, Double]
        shouldNotTypecheck
          (unionRemove u :: Either (Identity String) (Union Identity '[String, Double]))
    ]
