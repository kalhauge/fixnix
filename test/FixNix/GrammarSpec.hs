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
enumerationG = sumG EnumerationC
  { ifOne = "one"
  , ifTwo = "two"
  , ifThree = "three"
  }

spec :: Spec
spec = do
  describeGrammar "github"
    (detuple (until1G "git-owner" '/', until1G "git-repo" '/'))
    [ "hello/world/" ]
    ( liftM2 (,) (Gen.text (Range.linear 0 10) Gen.unicode) (Gen.text (Range.linear 0 10) Gen.unicode))

  describeGrammar "enumeration" 
    enumerationG
    [ "one" , "two" , "three" ]
    ( Gen.element [ One, Two, Three ] )

describeGrammar :: (Eq a, Show a) => String -> Grammar a -> [ Text ] -> Gen a -> Spec
describeGrammar name grm examples generator = describe ("Grammar " <> name) do
  describe "examples" do
    forM_ examples \from -> do
      it ("should parse " ++ Text.unpack from) do
        parse (parser grm) "GRAMMA" `shouldSucceedOn` from 
      it ("should parse-pretty-parse " ++ Text.unpack from) do
        let Right a = parse (parser grm) "" from
        counterexample (show a) do
          case prettyText grm a of 
            Right target -> 
              counterexample (show target) do
                parse (parser grm) "" target `shouldParse` a
            Left a -> 
              fail a
  describe "adjunction" do
    it "should be able to pretty-render-pretty" . hedgehog $ do
      a <- forAll generator
      case prettyText grm a of
        Left msg -> return ()
        Right b  -> do
          parse (parser grm) "grammar" b === Right a


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

