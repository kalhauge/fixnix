{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      :  FixNix.Grammar
-- Copyright   :  (c) Christian Gram Kalhauge 2020
-- License     :  BSD3
-- Maintainer  :  christian@kalhauge.dk
module FixNix.Grammar where

-- base
import           Data.Void
import           Data.Typeable
import           Data.String
import           Data.Bifunctor
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Functor
import           Data.Monoid
import           Data.Functor.Contravariant
import           Control.Monad
import           Control.Applicative

-- mtl
import           Control.Monad.Writer

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

-- grammar
import Control.Grammar

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
data LocationG a where
  TerminalG :: Text -> LocationG ()
  Simple   :: Text -> P a -> B a -> LocationG a
  Group    :: Text -> Doc -> LocationG a -> LocationG a
  ChooseG   :: LocationG (Either a a) -> LocationG a
  LocProdG :: ProdG LocationG a -> LocationG a
  LocSumG  :: SumG LocationG a -> LocationG a
  LocPIsoG  :: (b -> Maybe a) -> (a -> Maybe b) -> LocationG a -> LocationG b

instance HasIso LocationG where
  iso ba ab = LocPIsoG (Just . ba) (Just . ab)

instance HasProdG LocationG LocationG where
  prodG a = LocProdG (ProdG a)

instance HasSumG LocationG LocationG where
  sumG a = LocSumG (SumG a)

-- sumG ::
--   (Transformable (Church a), HasChurch a, NatFoldable (Church a))
--   => Church a Grammar
--   -> Grammar a
-- sumG = SumG
--
instance IsString (LocationG ()) where
  fromString = TerminalG . Text.pack


-- | We can parse a grammar
parser :: LocationG a -> () -> P a
parser grm = case grm of
  TerminalG t -> \() -> P.string t $> ()
  Group _ _ a -> parser a
  Simple _ pa _ -> \() -> pa
  LocProdG d -> unfoldProdG parser d
  LocSumG d -> unfoldSumG parser d
  ChooseG gaa -> \() -> either id id <$> parser gaa ()
  LocPIsoG _ ab ga -> \() -> maybe mzero pure . ab =<< parser ga ()

-- | We can pretty print a grammar
render :: LocationG a -> B a
render grm = case grm of
  TerminalG t ->
    fromText t
  Group _ _ a ->
    render a
  Simple _ _ b ->
    b
  LocProdG d ->
    Op (getAp . foldProdG (\ta a -> Ap $ getOp (render ta) a) d)
  LocSumG d ->
    Op (foldSumG (getOp . render) d)
  ChooseG gaa ->
    Op \a -> do
      let ea = getOp (render gaa)
      ea (Right a) <|> ea (Left a)

  LocPIsoG ba _ (render -> fa) ->
    Op (maybe (Left "nothing") (getOp fa) . ba)


prettyText :: LocationG a -> a -> Either String Text
prettyText = buildToText . render

prettyText' :: LocationG a -> a -> Text
prettyText' grm a = case buildToText (render grm) a of
  Right x -> x
  Left msg -> error $ "Unexpected error whilw printing:" ++ msg

parseText' :: LocationG a -> Text -> a
parseText' grm a = case parseEither (parser grm ()) "GRAMMA" a of
  Right x -> x
  Left msg -> error $ "Unexpected error while parseing:" ++ msg

-- | A missing utility of Megaparsec.
parseEither :: P a -> String -> Text -> Either String a
parseEither p str =
  first P.errorBundlePretty . P.parse p str

explain :: LocationG a -> Writer [(Text, Doc)] Doc
explain = \case
  TerminalG t -> return $ D.squote <> D.string (Text.unpack t) <> D.squote
  Simple txt _ _ -> return $ D.string (Text.unpack txt)
  ChooseG ga -> explain ga
  Group t d ga -> do
    doc <- explain ga
    tell [(t, d <> ":" <$$> doc)]
    return $ "<" <> D.string (Text.unpack t) <> ">"
  LocProdG d -> do
    x <- sequence $ inspectProdG explain d
    pure $ foldr (D.</>) mempty x
  LocSumG d -> do
    hl <- sequence $ inspectSumG explain d
    return $ D.encloseSep "(" ")" "|" hl
  LocPIsoG _ _ ga -> explain ga

explainGrammar :: LocationG a -> Doc
explainGrammar g =
  vsep
  [ nest 4 doc
  , ""
  , "where"
  , indent 2 $ vsep
    [ vsep [ "" , nest 4 $ "<" <> D.string (Text.unpack lt) <> ">" <+> "is" D.</> doc']
    | (lt, doc') <- contex
    ]
  ]
 where
   (doc, contex) = runWriter $ explain g

explainText :: LocationG a -> Text
explainText =
  Text.pack . flip displayS "" . renderPretty 0.9 80 . explainGrammar
--
untilG :: Text -> Char -> LocationG Text
untilG name csep = Simple ("<" <> name <> ">")
  (P.try $ P.takeWhileP Nothing (/= csep))
  (Op \txt -> if Text.all (/= csep) txt
    then Right $
      B.fromText txt
    else Left $
      "found " ++  show csep ++ " in " ++ show txt
      ++ ", but it is ended by " ++ show csep ++ "."
  )

restG :: Text -> LocationG Text
restG name = Simple ("<" <> name <> ">") P.takeRest (Op $ Right . B.fromText)

endG :: LocationG ()
endG = Simple "$" P.eof (Op $ Right . const "")

until1G :: Text -> Char -> LocationG Text
until1G name csep = Simple ("<" <> name <> ">")
  (P.try $ P.takeWhile1P Nothing (/= csep))
  (Op \txt -> if
    | not (Text.all (/= csep) txt) ->
      Left $ "found " ++  show csep ++ " in " ++ show txt
        ++ ", but it is ended by " ++ show csep ++ "."
    | Text.null txt ->
      Left $ show txt ++ "is empty but should have atleast one element"
    | otherwise ->
      Right $ B.fromText txt
  )


until1IncG :: Text -> Char -> LocationG Text
until1IncG name csep = Simple ("<" <> name <> "> '" <> Text.singleton csep <> "'")
  (P.try $ P.takeWhile1P Nothing (/= csep) <* P.char csep )
  (Op \txt -> if
    | not (Text.all (/= csep) txt) ->
      Left $ "found " ++  show csep ++ " in " ++ show txt
        ++ ", but it is ended by " ++ show csep ++ "."
    | Text.null txt ->
      Left $ show txt ++ "is empty but should have atleast one element"
    | otherwise ->
      Right $ B.fromText txt <> B.fromString [csep]
  )
--
-- eitherG :: Grammar a -> Grammar b -> Grammar (Either a b)
-- eitherG ifLeft ifRight = sumG EitherC {..}
--
-- maybeG :: Grammar a -> Grammar () -> Grammar (Maybe a)
-- maybeG ifJust ifNothing = sumG MaybeC {..}
--
-- Given an non-empty list of grammars combine them into one
anyG :: [LocationG a] -> LocationG a
anyG = \case
  [a] -> a
  a:as -> ChooseG $ defS CoEither
    { ifLeft = a
    , ifRight = anyG as
    }
  [] -> Simple "or fail" (fail "expected something else") (Op . const $ error "expected something else")

piso :: (b -> Maybe a) -> (a -> Maybe b) -> LocationG a -> LocationG b
piso = LocPIsoG

--
-- (...) = (.) . (.)
--
-- (**) :: Grammar a -> Grammar b -> Grammar (a, b)
-- (**) = ProductG
--
-- (**!) :: Grammar a -> Grammar () -> Grammar a
-- (**!) = IMap (\(a, ()) -> a) (\a -> (a, ())) ... ProductG
--
-- (!**) :: Grammar () -> Grammar a -> Grammar a
-- (!**) = IMap (\((), a) -> a) (\a -> ((), a)) ... ProductG
--
--
-- class Detuple a b | a -> b where
--   detuple :: a -> b
--
-- instance Detuple (Grammar a, Grammar b) (Grammar (a, b)) where
--   detuple (a, b) = ProductG a b
--
-- instance Detuple (Grammar a, Grammar b, Grammar c) (Grammar (a, b, c)) where
--   detuple (a, b, c) = IMap
--     (\(a, (b, c)) -> (a, b, c))
--     (\(a, b, c) -> (a, (b, c)))
--     (ProductG a (ProductG b c))
--
-- instance Detuple (Grammar a, Grammar b, Grammar c, Grammar d) (Grammar (a, b, c, d)) where
--   detuple (a, b, c, d) = IMap
--     (\(a, (b, (c, d))) -> (a, b, c, d))
--     (\(a, b, c, d) -> (a, (b, (c, d))))
--     (ProductG a (ProductG b (ProductG c d)))
