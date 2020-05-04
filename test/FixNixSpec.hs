{-# LANGUAGE BlockArguments #-}
module FixNixSpec where

import Test.Hspec
import Test.Hspec.Megaparsec

import Text.Megaparsec

import FixNix

spec :: Spec 
spec = do
  describe "location parser" do
    let 
      itShouldSucceedOn s = 
        it ("should succeed on " ++ show s) do
          parse locationP "" `shouldSucceedOn` s

      itShouldFailOn s = 
        it ("should fail on " ++ show s) do
          parse locationP "" `shouldFailOn` s

    describe "nixos" do
      itShouldSucceedOn "nixos:tags/20.03" 
      itShouldSucceedOn "nixos:heads/nixpkgs-20.03" 
      itShouldSucceedOn "nixos:AB023ad"
      itShouldSucceedOn "nixos:20.03"

      itShouldFailOn "nixos:a0232s"
      itShouldFailOn "nixos:eads/nixpkgs-20.03" 

    describe "github" do
      itShouldSucceedOn "github:nixos/nixpkgs/tags/20.03" 
      itShouldSucceedOn "github:nixos/nixpkgs/20.03" 

