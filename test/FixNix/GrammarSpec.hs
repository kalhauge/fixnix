{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
module FixNix.GrammarSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Megaparsec
import Test.Hspec.Hedgehog
import Test.Hspec.Expectations.Pretty

import Text.Megaparsec

import Data.Functor.Contravariant

import Data.Text (Text)
import qualified Data.Text as Text

-- import NeatInterpolation (text)
-- 
-- import Text.Megaparsec
-- 
-- import Control.Monad
-- import qualified Data.Text as Text

import Data.Text.Lazy.Builder as B

import Data.Foldable
-- import System.Exit
-- import qualified Data.List as L
-- 
-- import Options.Applicative
-- 
-- import Control.Exception
-- import Type.Reflection
-- 
-- import Test.HUnit.Lang
-- 
-- import FixNix
-- import FixNix.Core
-- import FixNix.Locations

import FixNix.Grammar
import FixNix.Grammar.Church

data Enumeration 
  = One
  | Two
  | Three
  deriving (Show, Eq)

data EnumerationC f = EnumerationC
  { ifOne :: f ()
  , ifTwo :: f ()
  , ifThree :: f ()
  }

instance HasChurch Enumeration where
  type Church Enumeration = EnumerationC

  interpC EnumerationC {..} = \case
    One -> getOp ifOne ()
    Two -> getOp ifTwo ()
    Three -> getOp ifThree ()

  generateC EnumerationC {..} = asum
    [ One <$ ifOne
    , Two <$ ifTwo
    , Three <$ ifThree
    ]

instance Transformable EnumerationC where
  transform nat EnumerationC {..} = 
    EnumerationC { ifOne = nat ifOne, ifTwo = nat ifTwo, ifThree = nat ifThree }

enumerationG :: Grammar Enumeration
enumerationG = productG EnumerationC
  { ifOne = "one"
  , ifTwo = "two"
  , ifThree = "three"
  }

spec :: Spec
spec = do
  describeGrammar "github"
    (detuple (untilG "git-owner" '/', untilG "git-repo" '/'))
    [ ("hello/world/", ("hello", "world"))
    ]
  
  describeGrammar "enumeration" 
    enumerationG
    [ ("one", One)
    , ("two", Two)
    , ("three", Three)
    ]


describeGrammar :: (Eq a, Show a) => String -> Grammar a -> [ (Text, a) ] -> PropertyT IO a -> Spec
describeGrammar name grm examples = describe ("Grammar " <> name) do
  describe "examples" do
    forM_ examples \(from, to) -> do
      it ("should parse " ++ (Text.unpack from)) do
        parse (parser grm) "GRAMMA" from `shouldParse` to
      it ("should pretty " ++ show to) do
        prettyText grm to `shouldBe` Right from
  describe "adjunction" do
    it "should be able to pretty-render-pretty" $ hedgehog \a ->
      case prettyText a of
        Left msg -> return ()
        Right b  -> do
          parse (parser grm) "grammar" b `shouldParse` a




  -- describe "parser" do 
  --   it "should parse 'hello'" do
  --     let 
  --       githubGrammar :: Grammar (Text, Text)
  --       githubGrammar = 
  --           detuple (var "git-owner" <+ "/", var "git-repo" <+ "/")
  --     parse (parser githubGrammar) "HELLO" `shouldSucceedOn` "hsello/this/"

