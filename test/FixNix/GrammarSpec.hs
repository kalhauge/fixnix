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

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Text.Megaparsec
import NeatInterpolation

import Data.Functor.Contravariant

import Data.Text (Text)
import Control.Monad
import qualified Data.Text as Text

import Data.Text.Lazy.Builder as B

import Data.Foldable

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

instance NatFoldable EnumerationC where
  foldN nat EnumerationC {..} = 
    nat ifOne <> nat ifTwo <> nat ifThree 

enumerationG :: Grammar Enumeration
enumerationG = productG EnumerationC
  { ifOne = "one"
  , ifTwo = "two"
  , ifThree = "three"
  }

spec :: Spec
spec = do
  describeGrammar "github"
    (detuple (until1G "git-owner" '/', until1G "git-repo" '/'))
    [ ("hello/world/", ("hello", "world"))
    ]
    ( liftM2 (,) (Gen.text (Range.linear 0 10) Gen.unicode) (Gen.text (Range.linear 0 10) Gen.unicode))
    "<git-owner>/<git-repo>/"

  describeGrammar "enumeration" 
    enumerationG
    [ ("one", One)
    , ("two", Two)
    , ("three", Three)
    ]
    ( Gen.element [ One, Two, Three ] )
    [text|<enumeration>

    where <enumeration> is 
      one
      two
      three
    |]

describeGrammar :: (Eq a, Show a) => String -> Grammar a -> [ (Text, a) ] -> Gen a -> Text -> Spec
describeGrammar name grm examples generator explained = describe ("Grammar " <> name) do
  describe "examples" do
    forM_ examples \(from, to) -> do
      it ("should parse " ++ (Text.unpack from)) do
        parse (parser grm) "GRAMMA" from `shouldParse` to
      it ("should pretty " ++ show to) do
        prettyText grm to `shouldBe` Right from
  describe "adjunction" do
    it "should be able to pretty-render-pretty" . hedgehog $ do
      a <- forAll generator
      case prettyText grm a of
        Left msg -> return ()
        Right b  -> do
          parse (parser grm) "grammar" b === Right a
  describe "explain" do
    it "should be explained" do
      explainText grm `shouldBe` explained




  -- describe "parser" do 
  --   it "should parse 'hello'" do
  --     let 
  --       githubGrammar :: Grammar (Text, Text)
  --       githubGrammar = 
  --           detuple (var "git-owner" <+ "/", var "git-repo" <+ "/")
  --     parse (parser githubGrammar) "HELLO" `shouldSucceedOn` "hsello/this/"

