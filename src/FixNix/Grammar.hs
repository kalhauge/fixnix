{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      :  FixNix.Grammar
-- Copyright   :  (c) Christian Gram Kalhauge 2020
-- License     :  BSD3
-- Maintainer  :  christian@kalhauge.dk
module FixNix.Grammar where

-- base 
import           Data.Void
import           Data.String
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Functor
import           Data.Monoid
import           Data.Functor.Contravariant
import           Control.Monad
import           Control.Applicative

-- text
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy                as LazyText
import           Data.Text                      ( Text )
import Data.Text.Lazy.Builder         ( Builder )
import qualified Data.Text.Lazy.Builder        as B

-- optparse-applicative
import           Options.Applicative.Help      as D 

-- megaparsec
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P

import FixNix.Grammar.Church

-- | A simple parser
type P = P.Parsec Void Text

type B = Op (Either String Builder)

fromText :: Text -> B ()
fromText txt = 
  Op . const . Right $ B.fromText txt

buildToText :: B a -> a -> Either String Text
buildToText (Op fa) = 
  fmap (LazyText.toStrict . B.toLazyText) . fa

-- | A grammar is a partial isomorphism between a and Text.
data Grammar a where
  Terminal :: Text -> Grammar ()
  Simple   :: Text -> P a -> B a -> Grammar a
  ProductG :: Grammar a -> Grammar b -> Grammar (a, b)
  ChooseG  :: Grammar (Either a a) -> Grammar a
  SumG     :: (Transformable (Church a), HasChurch a, NatFoldable (Church a)) 
           => Church a Grammar -> Grammar a
  IMap     :: (a -> b) -> (b -> a) -> Grammar a -> Grammar b

sumG :: 
  (Transformable (Church a), HasChurch a, NatFoldable (Church a)) 
  => Church a Grammar 
  -> Grammar a
sumG = SumG

instance IsString (Grammar ()) where
  fromString = Terminal . Text.pack

-- | We can parse a grammar
parser :: Grammar a -> P a
parser grm = case grm of
  Terminal t -> P.string t $> ()
  Simple txt pa ba -> pa 
  ProductG ga gb -> liftM2 (,) (parser ga) (parser gb)
  SumG d -> generateC (transform parser d)
  ChooseG gaa -> either id id <$> parser gaa
  IMap ab ba ga -> ab <$> parser ga

-- | We can pretty print a grammar
render :: Grammar a -> B a
render grm = case grm of
  Terminal t -> 
    fromText t
  Simple n _ b -> 
    b
  ProductG ga gb -> 
    Op \(a, b) -> 
      liftM2 (<>) (getOp (render ga) a) (getOp (render gb) b)
  ChooseG gaa -> 
    Op \a -> do
      let ea = getOp (render gaa)
      ea (Right a) <|> ea (Left a)

  SumG d -> 
    Op $ interpC (transform render d)
  IMap _ ba (render -> fa) -> 
    contramap ba fa

prettyText :: Grammar a -> a -> Either String Text
prettyText = buildToText . render 

explain :: Grammar a -> Doc
explain = \case
  Terminal t -> D.squote <> D.string (Text.unpack t) <> D.squote
  Simple txt pa ba -> D.string (Text.unpack txt)
  ProductG ga gb -> explain ga D.<+> explain gb
  ChooseG ga -> explain ga
  SumG d -> (D.encloseSep "(" ")" "|" $ foldN ((:[]) . explain) d)
  IMap ab ba ga -> explain ga

explainText :: Grammar a -> Text
explainText = 
  Text.pack . flip displayS "" . renderPretty 0.9 80 . explain

untilG :: Text -> Char -> Grammar Text
untilG name sep = Simple ("<" <> name <> "> '" <> Text.singleton sep <> "'")
  (P.try $ P.takeWhileP Nothing (/= sep) <* P.char sep) 
  (Op \txt -> if Text.all (/= sep) txt 
    then Right $ 
      B.fromText txt <> B.fromString [sep]
    else Left $
      "found " ++  show sep ++ " in " ++ show txt 
      ++ ", but it is ended by " ++ show sep ++ "."
  )

restG :: Text -> Grammar Text
restG name = Simple ("<" <> name <> ">") (P.takeRest) (Op $ Right . B.fromText)

until1G :: Text -> Char -> Grammar Text
until1G name sep = Simple ("<" <> name <> "> '" <> Text.singleton sep <> "'")
  (P.try $ P.takeWhile1P Nothing (/= sep) <* P.char sep) 
  (Op \txt -> if
    | not (Text.all (/= sep) txt) ->
      Left $ "found " ++  show sep ++ " in " ++ show txt 
        ++ ", but it is ended by " ++ show sep ++ "."
    | Text.null txt ->
      Left $ show txt ++ "is empty but should have atleast one element"
    | otherwise -> 
      Right $ B.fromText txt <> B.fromString [sep]
  )

eitherG :: Grammar a -> Grammar b -> Grammar (Either a b)
eitherG ifLeft ifRight = sumG EitherC {..}

maybeG :: Grammar a -> Grammar () -> Grammar (Maybe a)
maybeG ifJust ifNothing = sumG MaybeC {..}

-- Given an non-empty list of grammars combine them into one
anyG :: [Grammar a] -> Grammar a
anyG = \case 
  [a] -> a
  a:as -> ChooseG $ sumG EitherC 
    { ifLeft = a
    , ifRight = anyG as
    }
  [] -> Simple "or fail" (fail "expected something else") (Op (\a -> error "expected something else"))

(...) = (.) . (.)

(**) :: Grammar a -> Grammar b -> Grammar (a, b)
(**) = ProductG 

(**!) :: Grammar a -> Grammar () -> Grammar a
(**!) = IMap (\(a, ()) -> a) (\a -> (a, ())) ... ProductG 

(!**) :: Grammar () -> Grammar a -> Grammar a
(!**) = IMap (\((), a) -> a) (\a -> ((), a)) ... ProductG 


class Detuple a b | a -> b where
  detuple :: a -> b

instance Detuple (Grammar a, Grammar b) (Grammar (a, b)) where
  detuple (a, b) = ProductG a b

instance Detuple (Grammar a, Grammar b, Grammar c) (Grammar (a, b, c)) where
  detuple (a, b, c) = IMap 
    (\(a, (b, c)) -> (a, b, c))
    (\(a, b, c) -> (a, (b, c)))
    (ProductG a (ProductG b c))

instance Detuple (Grammar a, Grammar b, Grammar c, Grammar d) (Grammar (a, b, c, d)) where
  detuple (a, b, c, d) = IMap 
    (\(a, (b, (c, d))) -> (a, b, c, d))
    (\(a, b, c, d) -> (a, (b, (c, d))))
    (ProductG a (ProductG b (ProductG c d)))
