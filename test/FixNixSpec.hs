{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
module FixNixSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Megaparsec
import Test.Hspec.Expectations.Pretty

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

locationTypeSpec :: LocationType -> Spec
locationTypeSpec (LocationType {..}) = describe (Text.unpack locTypeName) do
  describe "parser" do
    forM_ locTypeExamples \Example {..} ->  
      it ("should succeed on " ++ show exampleIdentifier) do
        parse locTypeParser "" `shouldSucceedOn` exampleIdentifier

  describe "parser-printer adjunction" do
    forM_ locTypeExamples \Example {..} -> do
      it ("should parse-pretty-parse on " ++ show exampleIdentifier) do
        let Right a = parse locTypeParser "" exampleIdentifier
        counterexample (show a) do
          let target = finderIdentifier (locTypeFinder a)
          counterexample (show target) do
            parse locTypeParser "" target `shouldParse` a

spec :: Spec
spec = do
  describe "location types" do
    mapM_ locationTypeSpec locations 

  describe "renderFetchUrl" do
    it "should print something" do
      let 
        input = FetchUrl 
          { fetchUrl = "hello"
          , fetchName = Just "Here"
          , fetchUnpack = True
          }
        output = [text|builtins.fetchTarball {
          name   = "Here";
          url    = "hello";
          sha256 = "0000000000000000000000000000000000000000000000000000";
        }|]
      renderFetchUrl input zeroSha256 `shouldBe` output
    it "should be able to not print name" do
      let 
        input = FetchUrl 
          { fetchUrl = "hello"
          , fetchName = Nothing
          , fetchUnpack = True
          }
        output = [text|builtins.fetchTarball {
          url    = "hello";
          sha256 = "0000000000000000000000000000000000000000000000000000";
        }|]
      renderFetchUrl input zeroSha256 `shouldBe` output
  
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


