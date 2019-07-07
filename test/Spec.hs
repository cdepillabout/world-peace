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
            removed = unionRemove u :: Either (Union Identity '[]) (Identity String)
        removed @?= Right (Identity "hello")
    , testCase "fail to match final element" $ do
        let u = This (Identity "hello") :: Union Identity '[String]
            removed = unionRemove u :: Either (Union Identity '[String]) (Identity Double)
        removed @?= Left u
    , testCase "match leading non-final element" $ do
        let u = This (Identity "hello") :: Union Identity '[String, Double]
            removed = unionRemove u :: Either (Union Identity '[Double]) (Identity String)
        removed @?= Right (Identity "hello")
    , testCase "fail match leading non-final element" $ do
        let u = That (This (Identity 3.5)) :: Union Identity '[String, Double]
            removed = unionRemove u :: Either (Union Identity '[Double]) (Identity String)
        removed @?= Left (This (Identity 3.5))
    , testCase "match non-leading non-final element" $ do
        let u = That (This (Identity "hello")) :: Union Identity '[Char, String, Double]
            removed = unionRemove u :: Either (Union Identity '[Char, Double]) (Identity String)
        removed @?= Right (Identity "hello")
    , testCase "fail match non-leading non-final element 1" $ do
        let u = That (That (This (Identity 3.5))) :: Union Identity '[Char, String, Double]
            removed = unionRemove u :: Either (Union Identity '[Char, Double]) (Identity String)
        removed @?= Left (That (This (Identity 3.5)))
    , testCase "fail match non-leading non-final element 2" $ do
        let u = This (Identity 'c') :: Union Identity '[Char, String, Double]
            removed = unionRemove u :: Either (Union Identity '[Char, Double]) (Identity String)
        removed @?= Left (This (Identity 'c'))
    , testCase "fail match non-existing element" $ do
        let u = This (Identity 'c') :: Union Identity '[Char, String, Double]
            removed = unionRemove u :: Either (Union Identity '[Char, String, Double]) (Identity Float)
        removed @?= Left u
    , testCase "match multiple 1" $ do
        let u = This (Identity 'c') :: Union Identity '[Char, Char]
            removed = unionRemove u :: Either (Union Identity '[]) (Identity Char)
        removed @?= Right (Identity 'c')
    , testCase "match multiple 2" $ do
        let u = That (This (Identity 'c')) :: Union Identity '[Char, Char]
            removed = unionRemove u :: Either (Union Identity '[]) (Identity Char)
        removed @?= Right (Identity 'c')
    , testCase "match multiple 3" $ do
        let u = That (This (Identity 'c')) :: Union Identity '[Double, Char, String, Char, Float]
            removed = unionRemove u :: Either (Union Identity '[Double, String, Float]) (Identity Char)
        removed @?= Right (Identity 'c')
    , testCase "fail to match multiple 1" $ do
        let u = That (That (This (Identity 3.5))) :: Union Identity '[Char, Char, Double]
            removed = unionRemove u :: Either (Union Identity '[Double]) (Identity Char)
        removed @?= Left (This (Identity 3.5))
    , testCase "fail to match multiple 2" $ do
        let u = That (That (This (Identity 3.5))) :: Union Identity '[String, Char, Float, Char, Double]
            removed = unionRemove u :: Either (Union Identity '[String, Float, Double]) (Identity Char)
        removed @?= Left (That (This (Identity 3.5)))
    , testCase "type inference works somewhat 1" $ do
        let u = This (Identity 3.5) :: Union Identity '[Double, Char, Int]
        unionRemove u @?= Right (Identity (3.5 :: Double))
    , testCase "type inference works somewhat 2" $ do
        let u = That (This (Identity 'c')) :: Union Identity '[Double, Char, Int]
        unionRemove u @?= Right (Identity ('c'))
    ]
