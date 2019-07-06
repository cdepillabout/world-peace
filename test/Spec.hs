{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Functor.Identity (Identity(Identity))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Data.WorldPeace (Union(..), unionRemove)
import Test.TypeErrors (unionRemoveTypeErrors)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "tests"
    [ unionRemoveTests
    , unionRemoveTypeErrors
    ]

unionRemoveTests :: TestTree
unionRemoveTests =
  testGroup
    "unionRemove"
    [ testCase "match final element" $ do
        let u = This (Identity "hello") :: Union Identity '[String]
            removed = unionRemove u :: Either (Identity String) (Union Identity '[])
        removed @?= Left (Identity "hello")
    , testCase "fail to match final element" $ do
        let u = This (Identity "hello") :: Union Identity '[String]
            removed = unionRemove u :: Either (Identity Double) (Union Identity '[String])
        removed @?= Right u
    , testCase "match leading non-final element" $ do
        let u = This (Identity "hello") :: Union Identity '[String, Double]
            removed = unionRemove u :: Either (Identity String) (Union Identity '[Double])
        removed @?= Left (Identity "hello")
    , testCase "fail match leading non-final element" $ do
        let u = That (This (Identity 3.5)) :: Union Identity '[String, Double]
            removed = unionRemove u :: Either (Identity String) (Union Identity '[Double])
        removed @?= Right (This (Identity 3.5))
    , testCase "match non-leading non-final element" $ do
        let u = That (This (Identity "hello")) :: Union Identity '[Char, String, Double]
            removed = unionRemove u :: Either (Identity String) (Union Identity '[Char, Double])
        removed @?= Left (Identity "hello")
    , testCase "fail match non-leading non-final element 1" $ do
        let u = That (That (This (Identity 3.5))) :: Union Identity '[Char, String, Double]
            removed = unionRemove u :: Either (Identity String) (Union Identity '[Char, Double])
        removed @?= Right (That (This (Identity 3.5)))
    , testCase "fail match non-leading non-final element 2" $ do
        let u = This (Identity 'c') :: Union Identity '[Char, String, Double]
            removed = unionRemove u :: Either (Identity String) (Union Identity '[Char, Double])
        removed @?= Right (This (Identity 'c'))
    , testCase "fail match non-existing element" $ do
        let u = This (Identity 'c') :: Union Identity '[Char, String, Double]
            removed = unionRemove u :: Either (Identity Float) (Union Identity '[Char, String, Double])
        removed @?= Right u
    , testCase "match multiple 1" $ do
        let u = This (Identity 'c') :: Union Identity '[Char, Char]
            removed = unionRemove u :: Either (Identity Char) (Union Identity '[])
        removed @?= Left (Identity 'c')
    , testCase "match multiple 2" $ do
        let u = That (This (Identity 'c')) :: Union Identity '[Char, Char]
            removed = unionRemove u :: Either (Identity Char) (Union Identity '[])
        removed @?= Left (Identity 'c')
    , testCase "match multiple 3" $ do
        let u = That (This (Identity 'c')) :: Union Identity '[Double, Char, String, Char, Float]
            removed = unionRemove u :: Either (Identity Char) (Union Identity '[Double, String, Float])
        removed @?= Left (Identity 'c')
    , testCase "fail to match multiple 1" $ do
        let u = That (That (This (Identity 3.5))) :: Union Identity '[Char, Char, Double]
            removed = unionRemove u :: Either (Identity Char) (Union Identity '[Double])
        removed @?= Right (This (Identity 3.5))
    , testCase "fail to match multiple 2" $ do
        let u = That (That (This (Identity 3.5))) :: Union Identity '[String, Char, Float, Char, Double]
            removed = unionRemove u :: Either (Identity Char) (Union Identity '[String, Float, Double])
        removed @?= Right (That (This (Identity 3.5)))
    ]
