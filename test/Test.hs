{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Scrub

import           Data.String
import           Data.Typeable
import           GHC.Generics

import           Test.Hspec
import           Data.Semigroup (Semigroup)


-- Nested Generics

data NestedInner = NestedInner
  { abc :: Email
  , def :: Maybe Email
  } deriving (Eq, Show, Generic)
data Nested = Nested { ghi :: NestedInner, jkl :: String } deriving (Eq, Show, Generic)
instance Scrub NestedInner
instance Scrub Nested
nestedGenerics :: Spec
nestedGenerics =  it "subdata should get filled when derived from generics" $
  let n = Nested (NestedInner "f@g.c" (Just "f@g.c")) "foo"
  in scrub n `shouldBe` Scrubbed (Nested (NestedInner "<Scrubbed Email>" (Just "<Scrubbed Email>")) "foo")


-- Stringy Newtypes

newtype DangerZone = DangerZone String deriving (Show, Eq, Typeable, Semigroup, Monoid, IsString)
instance Scrub DangerZone where scrub = scrubS
stringIsh :: Spec
stringIsh = it "should include type information when using scrubS" $
  let s = "highway to the" :: DangerZone
  in scrub s `shouldBe` Scrubbed "<Scrubbed DangerZone>"


-- Numberish Newtypes

newtype ComfortablyNum = ComfortablyNum Float deriving (Show, Eq, Num)
instance Scrub ComfortablyNum where scrub _ = scrubI
numberIsh :: Spec
numberIsh = it "should scrub to -1 as the centinal value for a scrubbed number" $
  let ihavebecome = 123 :: ComfortablyNum
  in scrub ihavebecome `shouldBe` Scrubbed (-1)


-- Scrub a functor

functorScrub :: Spec
functorScrub = it "should be able to scrub any functor containing a scrubbable" $
  let xs = [Left "f@g.c", Right 15, Left "g@f.c"] :: [Either Email Passport]
  in scrub xs `shouldBe` Scrubbed [Left "<Scrubbed Email>", Right (-1), Left "<Scrubbed Email>"]


-- Smash all tests together

main :: IO ()
main = hspec $ do
  nestedGenerics
  stringIsh
  numberIsh
  functorScrub
