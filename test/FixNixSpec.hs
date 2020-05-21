{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
module FixNixSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Megaparsec
import Test.Hspec.Expectations.Pretty
import qualified Hedgehog.Gen as Gen

import NeatInterpolation (text)

import Text.Megaparsec

import Control.Monad
import qualified Data.Text as Text

import Data.Foldable
import System.Exit
import qualified Data.List as L

import Options.Applicative

import Control.Exception
import Type.Reflection

import Test.HUnit.Lang

import FixNix
import FixNix.Core
import FixNix.Locations

import FixNix.GrammarSpec

locationTypeSpec :: LocationType -> Spec
locationTypeSpec (LocationType {..}) = describe (Text.unpack locTypeName) do
  describeGrammar 
    (Text.unpack locTypeName)
    locTypeGrammar
    (map exampleIdentifier locTypeExamples)
    Nothing

--     forM_ locTypeExamples \Example {..} ->  
--       it ("should succeed on " ++ show exampleIdentifier) do
--         parse locTypeParser "" `shouldSucceedOn` exampleIdentifier
-- 
--   describe "parser-printer adjunction" do
--     forM_ locTypeExamples \Example {..} -> do
--       it ("should parse-pretty-parse on " ++ show exampleIdentifier) do
--         let Right a = parse locTypeParser "" exampleIdentifier
--         counterexample (show a) do
--           let target = finderIdentifier (locTypeFinder a)
--           counterexample (show target) do
--             parse locTypeParser "" target `shouldParse` a

spec :: Spec
spec = do
  describe "location types" do
    mapM_ locationTypeSpec locations 

  describe "renderFetchUrl" do
    it "should print something" do
      let 
        input = Location
          { locUrl = "hello"
          , locName = "name"
          , locMode = Unpack
          }
        output = [text|builtins.fetchTarball {
          name   = "name";
          url    = "hello";
          sha256 = "0000000000000000000000000000000000000000000000000000";
        }|]
      renderLocation input zeroSha256 `shouldBe` output

    it "should also support printing" do
      let 
        input = Location
          { locUrl = "hello"
          , locName = "name"
          , locMode = Import
          }
        output = [text|import (builtins.fetchTarball {
          name   = "name";
          url    = "hello";
          sha256 = "0000000000000000000000000000000000000000000000000000";
        })|]
      renderLocation input zeroSha256 `shouldBe` output

  describe "description" do
    fs <- runIO (readFile "USAGE.txt")
    it "should equal the description in USAGE.txt" do
      let a = execParserPure (prefs mempty) (fixnixParserInfo locations) ["-h"] 
      case a of 
        Options.Applicative.Failure failure -> do
          let (txt, s) = renderFailure failure "fixnix" 
          txt ++ "\n" `shouldBe` fs
          s `shouldBe` ExitSuccess
        _ -> 
          fail "Expected failure"

  describe "util functions" do
    describe "diffText" do
      it "equal texts should return nothing" do
        diffText False "" "" `shouldBe` Nothing
      it "should print nice things" do
        diffText False "hello" "you" `shouldBe` Just "--- hello\n+++ you\n"


