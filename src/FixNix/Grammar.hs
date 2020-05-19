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
import           Options.Applicative.Help    as D hiding ((<+>)) 

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
  SumG     :: Grammar a -> Grammar b -> Grammar (a, b)
  ProductG     :: (Transformable (Church a), HasChurch a, NatFoldable (Church a))
    => Church a Grammar -> Grammar a
  IMap     :: (a -> b) -> (b -> a) -> Grammar a -> Grammar b

productG :: 
  (Transformable (Church a), HasChurch a, NatFoldable (Church a)) 
  => Church a Grammar 
  -> Grammar a
productG = ProductG

instance IsString (Grammar ()) where
  fromString = Terminal . Text.pack

-- | We can parse a grammar
parser :: Grammar a -> P a
parser grm = case grm of
  Terminal t -> P.string t $> ()
  Simple txt pa ba -> pa 
  SumG ga gb -> liftM2 (,) (parser ga) (parser gb)
  ProductG d -> generateC (transform parser d)
  IMap ab ba ga -> ab <$> parser ga

-- | We can pretty print a grammar
render :: Grammar a -> B a
render grm = case grm of
  Terminal t   -> fromText t
  Simple n _ b -> b
  SumG ga gb   -> Op \(a, b) -> liftM2 (<>) (getOp (render ga) a) (getOp (render gb) b)
  ProductG d   -> Op $ interpC (transform render d)
  IMap _ ba (render -> fa) -> 
    contramap ba fa

prettyText :: Grammar a -> a -> Either String Text
prettyText = buildToText . render 

explain :: Grammar a -> Doc
explain = \case
  Terminal t -> D.string (Text.unpack t)
  Simple txt pa ba -> D.string (Text.unpack txt)
  SumG ga gb -> explain ga <> explain gb
  ProductG d -> foldN explain d
  IMap ab ba ga -> explain ga

explainText :: Grammar a -> Text
explainText = Text.pack . flip displayS "" . renderCompact . explain


untilG :: Text -> Char -> Grammar Text
untilG name sep = Simple ("<" <> name <> ">" <> Text.singleton sep)
  (P.takeWhileP Nothing (/= sep) <* P.char sep) 
  (Op \txt -> if Text.all (/= sep) txt 
    then Right $ 
      B.fromText txt <> B.fromString [sep]
    else Left $
      "found " ++  show sep ++ " in " ++ show txt 
      ++ ", but it is ended by " ++ show sep ++ "."
  )

until1G :: Text -> Char -> Grammar Text
until1G name sep = Simple ("<" <> name <> ">" <> Text.singleton sep) 
  (P.takeWhile1P Nothing (/= sep) <* P.char sep) 
  (Op \txt -> if
    | not (Text.all (/= sep) txt) ->
      Left $ "found " ++  show sep ++ " in " ++ show txt 
        ++ ", but it is ended by " ++ show sep ++ "."
    | Text.null txt ->
      Left $ show txt ++ "is empty but should have atleast one element"
    | otherwise -> 
      Right $ B.fromText txt <> B.fromString [sep]
  )

class Detuple a b | a -> b where
  detuple :: a -> b

instance Detuple (Grammar a, Grammar b) (Grammar (a, b)) where
  detuple (a, b) = SumG a b

instance Detuple (Grammar a, Grammar b, Grammar c) (Grammar (a, b, c)) where
  detuple (a, b, c) = IMap 
    (\(a, (b, c)) -> (a, b, c))
    (\(a, b, c) -> (a, (b, c)))
    (SumG a (SumG b c))

instance Detuple (Grammar a, Grammar b, Grammar c, Grammar d) (Grammar (a, b, c, d)) where
  detuple (a, b, c, d) = IMap 
    (\(a, (b, (c, d))) -> (a, b, c, d))
    (\(a, b, c, d) -> (a, (b, (c, d))))
    (SumG a (SumG b (SumG c d)))
