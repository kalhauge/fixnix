{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
module FixNixSpec where

import Test.Hspec
import Test.Hspec.Megaparsec

import NeatInterpolation (text)

import Text.Megaparsec

import Control.Monad
import qualified Data.Text as Text

import FixNix

spec :: Spec 
spec = do
  describe "location parser" do
    forM_ locations \LocationType {..} -> do
      describe (Text.unpack locationName) do
        forM_ locationExamples \LocationExample {..} ->  
          it ("should succeed on " ++ show exampleText) do
            parse locationParser "" `shouldSucceedOn` exampleText
      -- let 
      --   itShouldSucceedOn s = 
      --     it ("should succeed on " ++ show s) do
      --       parse locationParser "" `shouldSucceedOn` s

      --   itShouldFailOn s = 
      --     it ("should fail on " ++ show s) do
      --       parse locationParser "" `shouldFailOn` s


      -- describe "nixos" do
      --   itShouldSucceedOn "nixos:tags/20.03" 
      --   itShouldSucceedOn "nixos:heads/nixpkgs-20.03" 
      --   itShouldSucceedOn "nix:heads/nixpkgs-20.03" 

      --   -- itShouldSucceedOn "nixos:AB023ad"
      --   -- itShouldSucceedOn "nixos:20.03"

      --   itShouldFailOn "nixos:tag/20"
      --   itShouldFailOn "nixos:eads/nixpkgs-20.03" 
      --   itShouldFailOn "nixos:tags/"

      -- describe "github" do
      --   itShouldSucceedOn "github:nixos/nixpkgs/tags/20.03" 
      --   itShouldSucceedOn "github:nixos/nixpkgs/heads/nixpkgs-20.03" 
      --   itShouldSucceedOn "gh:kalhauge/fixnix/rev/abcdef" 

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

