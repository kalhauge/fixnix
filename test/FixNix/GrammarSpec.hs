{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Test.HUnit.Lang

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Text.Megaparsec
import NeatInterpolation

import Data.Functor.Contravariant
import Type.Reflection
import Control.Exception

import Data.Text (Text)
import Control.Monad
import qualified Data.Text as Text

import qualified Data.List as L

import Data.Text.Lazy.Builder as B

import Data.Foldable

-- grammar
import Control.Grammar
import Control.Grammar.TH

import FixNix.Grammar

data Enumeration
  = EOne
  | ETwo
  | EThree
  deriving (Show, Eq)

$(makeCoLimit ''Enumeration)

spec :: Spec
spec = do
  describeGrammar "github"
    (defP $ Two (until1G "git-owner" '/') (until1G "git-repo" '/'))
    [ "hello/world/" ]
    ( Just $ liftM2 (,)
      (Gen.text (Range.linear 0 10) Gen.unicode)
      (Gen.text (Range.linear 0 10) Gen.unicode)
    )

  describeGrammar "enumeration"
    (defS $ EnumerationCoLim "one" "two" "three")
    [ "one" , "two" , "three" ]
    ( Just $ Gen.element [ EOne, ETwo, EThree ] )

describeGrammar :: (Eq a, Show a) => String -> LocationG a -> [ Text ] -> (Maybe (Gen a)) -> Spec
describeGrammar name grm examples mgenerator = describe ("Grammar " <> name) do
  describe "examples" do
    forM_ examples \from -> do
      it ("should parse " ++ Text.unpack from) do
        parse (parser grm ()) "GRAMMA" `shouldSucceedOn` from
      it ("should parse-pretty-parse " ++ Text.unpack from) do
        let Right a = parse (parser grm ()) "" from
        counterexample (show a) do
          case prettyText grm a of
            Right target ->
              counterexample (show target) do
                parse (parser grm ()) "" target `shouldParse` a
            Left a ->
              fail a
  case mgenerator of
    Nothing -> return ()
    Just generator -> describe "adjunction" do
      it "should be able to pretty-render-pretty" . hedgehog $ do
        a <- forAll generator
        case prettyText grm a of
          Left msg -> return ()
          Right b  -> do
            parse (parser grm ()) "grammar" b === Right a


  -- describe "parser" do
  --   it "should parse 'hello'" do
  --     let
  --       githubGrammar :: Grammar (Text, Text)
  --       githubGrammar =
  --           detuple (var "git-owner" <+ "/", var "git-repo" <+ "/")
  --     parse (parser githubGrammar) "HELLO" `shouldSucceedOn` "hsello/this/"

data WithCounterExample =
  WithCounterExample [String] SomeException

instance Show WithCounterExample where
  show (WithCounterExample examples e@(SomeException ex)) =
    "Counter examples: " ++ "\n" ++
      fold (L.zipWith (\i str -> " " ++ show i ++ ") " ++ str ++ "\n") [1..] examples)
    ++ case fromException e of
         Just (HUnitFailure a reason) -> formatFailureReason reason
         Nothing -> show (typeOf ex) ++ ": " ++ show ex

instance Exception WithCounterExample where

counterexample :: HasCallStack => String -> IO a -> IO a
counterexample str io = catches io
  [ Handler (\(WithCounterExample strs e) -> throwIO (WithCounterExample (str:strs) e))
  , Handler (\e -> throwIO (WithCounterExample [str] e))
  ]

