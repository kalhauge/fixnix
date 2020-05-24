{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
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
-- Module      :  FixNix.Grammar.Church
-- Copyright   :  (c) Christian Gram Kalhauge 2020
-- License     :  BSD3
-- Maintainer  :  christian@kalhauge.dk
module FixNix.Grammar.Church where

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

-- adjunctions
import Data.Functor.Contravariant.Rep

-- optparse-applicative
import           Options.Applicative.Help   hiding ((<+>))

-- megaparsec
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P


-- | A church encoding of a type
class HasChurch a where
  type Church a :: (* -> *) -> *

  -- | Use the Church encoding as a interpreter
  interpC   :: Church a (Op b) -> a -> b

  -- | Use the Church encoding as a generator
  generateC :: Alternative m => Church a m -> m a

-- | The church encoding of a maybe
data MaybeC a f = MaybeC 
  { ifJust    :: f a
  , ifNothing :: f ()
  }

instance HasChurch (Maybe a) where
  type Church (Maybe a) = MaybeC a

  interpC MaybeC {..} m = case m of
    Just a -> getOp ifJust a
    Nothing -> getOp ifNothing ()

  generateC MaybeC {..} = 
    Just <$> ifJust <|> (Nothing <$ ifNothing)

-- | The church encoding of an Either
data EitherC a b f = EitherC 
  { ifLeft  :: f a
  , ifRight :: f b
  }

instance HasChurch (Either a b) where
  type Church (Either a b) = EitherC a b

  interpC EitherC {..} m = case m of
    Left  a -> getOp ifLeft a
    Right b -> getOp ifRight b 

  generateC EitherC {..} = 
    Left <$> ifLeft <|> (Right <$> ifRight)

class Transformable (k :: (* -> *) -> *) where
  transform :: (forall a. g a -> h a) -> k g -> k h

instance Transformable (EitherC a b) where
  transform nat EitherC {..} = 
    EitherC { ifLeft = nat ifLeft, ifRight = nat ifRight }

instance Transformable (MaybeC a) where
  transform nat MaybeC {..} = 
    MaybeC { ifJust = nat ifJust, ifNothing = nat ifNothing }

class NatFoldable (k :: (* -> *) -> *) where
  foldN :: Semigroup m => (forall a. g a -> m) -> k g -> m

instance NatFoldable (EitherC a b) where
  foldN nat EitherC {..} = nat ifLeft <> nat ifRight

instance NatFoldable (MaybeC b) where
  foldN nat MaybeC {..} = nat ifJust <> nat ifNothing

