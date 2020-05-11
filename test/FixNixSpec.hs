{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
module FixNixSpec where

import Test.Hspec
import Test.Hspec.Megaparsec

import NeatInterpolation (text)

import Text.Megaparsec

import Control.Monad
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as Builder

import Data.Foldable
import qualified Data.List as L

import Control.Exception
import Type.Reflection

import Test.HUnit.Lang

import FixNix

data WithCounterExample = WithCounterExample [String] SomeException 

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

locationTypeSpec :: AnyLocationType -> Spec
locationTypeSpec (AnyLocationType (LocationType {..})) = describe (Text.unpack locName) do
  describe "parser" do
    forM_ locExamples \LocationExample {..} ->  
      it ("should succeed on " ++ show exampleText) do
        parse locParser "" `shouldSucceedOn` exampleText

  describe "parser-printer adjunction" do
    forM_ locExamples \LocationExample {..} -> do
      it ("should parse-pretty-parse on " ++ show exampleText) do
        let Right a = parse locParser "" exampleText 
        counterexample (show a) do
          let target = LazyText.toStrict (Builder.toLazyText $ locPrinter a)
          counterexample (show target) do
            parse locParser "" target `shouldParse` a

spec :: Spec 
spec = do
  describe "location types" do
    mapM_ locationTypeSpec locations 

  describe "renderFetchUrl" do
    it "should print something" do
      let 
        input = FetchUrl 
          { fetchUrlUrl = "hello"
          , fetchUrlName = Just "Here"
          , fetchUrlUnpack = True
          }
        output = [text|builtins.fetchurl {
          name   = "Here";
          url    = "hello";
          sha256 = "0000000000000000000000000000000000000000000000000000";
        }|]
      renderFetchUrl input zeroSha256 `shouldBe` output
    it "should be able to not print name" do
      let 
        input = FetchUrl 
          { fetchUrlUrl = "hello"
          , fetchUrlName = Nothing
          , fetchUrlUnpack = True
          }
        output = [text|builtins.fetchurl {
          url    = "hello";
          sha256 = "0000000000000000000000000000000000000000000000000000";
        }|]
      renderFetchUrl input zeroSha256 `shouldBe` output

