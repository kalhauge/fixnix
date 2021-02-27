{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      :  FixNix.Core
-- Copyright   :  (c) Christian Gram Kalhauge 2020
-- License     :  BSD3
-- Maintainer  :  christian@kalhauge.dk
--
-- FixNix is a small tool for fixing versions of nixpkgs and other
-- sources used when writing nix-expressions.
module FixNix.Core where

-- base
import           Data.Bifunctor
import           Data.Maybe
import           Data.Typeable
import           Data.Functor
import           Control.Applicative
import           Data.Functor.Contravariant
import           Data.Void
import           Data.Foldable
import qualified Data.List.NonEmpty as NE
import           System.Exit

-- Diff
import Data.Algorithm.Diff
-- import Data.Algorithm.DiffOutput

-- text
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy                as LazyText
import qualified Data.Text.Lazy.Encoding       as LazyText

-- megaparsec
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P

-- ansi-terminal
import System.Console.ANSI

-- neat-interpolation
import           NeatInterpolation              as Neat ( text )

-- optparse-applicative
import           Options.Applicative.Help.Pretty as D hiding (text)
import qualified Options.Applicative.Help.Pretty as D

-- typed-process
import           System.Process.Typed

-- path
import Path

-- grammar
import Control.Grammar
import Control.Grammar.TH

-- fixnix
import FixNix.Grammar

data Config = Config
  { cfgFixFolder      :: !(Path Rel Dir)
  , cfgForce          :: !Bool
  }

data LocationMode
  = Download
  -- ^ Download the location
  | Unpack
  -- ^ Download and unpack the location
  | Import
  -- ^ Download, Unpack, and import the location
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | A location is a url with a name
data Location = Location
  { locName :: !Text
  -- ^ the suffix to add to the name
  , locUrl  :: !Text
  -- ^ the url of the location
  , locMode :: !LocationMode
  -- ^ what mode of download is needed
  } deriving (Show, Eq)

-- | A location builder is enough to build a location
data LocationBuilder = LocationBuilder
  { locBSuffix :: !(Maybe Text)
  -- ^ the suffix to add to the name
  , locBUrl    :: !Text
  -- ^ the url of the location
  } deriving (Show, Eq)

-- | A location finder either knows the location of the data, or knows
-- how to find the location.
data LocationFinder  = LocationFinder
  { finderBaseName :: !Text
    -- ^ The base name of the location, can be used to find filenames.
  , finderLocationMode :: !LocationMode
    -- ^ If the finder should be unpack
  , finderLocation :: Either (IO LocationBuilder) LocationBuilder
    -- ^ The url of the location, might need an impure computation.
  }

data TypedLocationFinder = forall a. (Show a, Eq a, Typeable a) => TypedLocationFinder
  { finderLocationData     :: a
  , finderLocationTypeName :: Text.Text
  , finderLocationFinder   :: LocationFinder
  }

deriving instance Show TypedLocationFinder

data LocationType = forall a. (Show a, Eq a, Typeable a) => LocationType (UnpackedLocationType a)

-- | To define an new location type we use this data structure.
data UnpackedLocationType a = UnpackedLocationType
  { locTypeName          :: Text
  -- ^ The name of the location type
  , locTypePrefix        :: NE.NonEmpty Text
  -- ^ The list of prefix used for invoking this type
  , locTypeDocumentation :: Doc
  -- ^ A documentation string
  , locTypeExamples      :: [ Example ]
  -- ^ A list of examples, that is both checked and presented to the user.
  , locTypeCompleter     :: Config -> String -> IO [ String ]
  -- ^ A function to complete the grammar
  , locTypeGrammar       :: LocationG a
  -- ^ A loc type printer-parser
  , locTypeFinder        :: a -> LocationFinder
  }

-- | An Example is an identifier and a some help text.
data Example = Example
  { exampleIdentifier :: Text
  -- ^ The identifier for location, should parse by the `locTypeParser`.
  , exampleHelp       :: D.Doc
  -- ^ The help string to understand the example.
  }


instance Show LocationFinder where
 show LocationFinder {..} = Text.unpack finderBaseName

cfgFilename :: Config -> LocationFinder -> Path Rel File
cfgFilename Config {..} LocationFinder {..} = case parseRelFile fname of
  Just rf -> cfgFixFolder Path.</> rf
  Nothing -> error ("Bad base name " ++ fname)
  where fname = Text.unpack $ finderBaseName <> ".nix"


-- | Find a location
buildLocation :: LocationFinder -> LocationBuilder -> Location
buildLocation LocationFinder {..} LocationBuilder {..} = Location
  { locName   = finderBaseName <> foldMap ("_" <>) locBSuffix
  , locUrl    = locBUrl
  , locMode = finderLocationMode
  }

-- | Find a location
findLocation :: LocationFinder -> IO Location
findLocation lf = fmap (buildLocation lf) . either id return $
  finderLocation lf

$(makeCoLimit ''LocationMode)
$(makeLimit ''LocationFinder)

-- | Grammar types
finderG :: [LocationType] -> LocationG TypedLocationFinder
finderG ltps =
  iso
    (\tlf@(TypedLocationFinder _ _ LocationFinder {..}) ->
      ( Just finderBaseName
      , Just finderLocationMode
      , tlf
      )
    )
    (\(name, mode, tlf) -> tlf
      { finderLocationFinder = let lf = finderLocationFinder tlf in lf
        { finderBaseName     = fromMaybe (finderBaseName lf) name
        , finderLocationMode = fromMaybe (finderLocationMode lf) mode
        }
      }
    )
  $ defP $ Three nameG locationModeG anyTypedLocationG
 where
  nameG :: LocationG (Maybe Text)
  nameG = defS CoMaybe
    { ifJust = until1IncG "name" '='
    , ifNothing = ""
    }

  locationModeG :: LocationG (Maybe LocationMode)
  locationModeG = Group "mode" "the download mode of the location" $ defS CoMaybe
    { ifJust = buildSum \LocationModeCoLim {..} ->
      [ ifDownload =: "@"
      , ifImport   =: "%"
      , ifUnpack   =: "+"
      ]
    , ifNothing = ""
    }

  anyTypedLocationG :: LocationG TypedLocationFinder
  anyTypedLocationG = Group "location" "any location" $ anyG
    [ case tp of
        LocationType up ->
          piso
            (\TypedLocationFinder{..} ->
              if locTypeName up == finderLocationTypeName
              then cast finderLocationData
              else Nothing
            )
            (\fld -> Just $ TypedLocationFinder fld (locTypeName up) (locTypeFinder up fld))
          $ anyG [ TerminalG t | t <- NE.toList (locTypePrefix up)]
            **> ":"
            **> Group (locTypeName up) (locTypeDocumentation up) (locTypeGrammar up)
    | tp <- ltps
    ]


-- | Pretty print the location type with examples
describeLocationType :: LocationType -> Doc
describeLocationType (LocationType UnpackedLocationType {..}) = vcat
  [ hsep [ "##", ttext locTypeName, prefixList ]
  , ""
  , indent 4 $ explainGrammar locTypeGrammar
  , ""
  , locTypeDocumentation
  , ""
  , "### Examples:"
  , ""
  , vsep $
    [ vsep
      [ e
      , ""
      , indent 4 . vsep $
        [ hsep
          [ "$", "fixnix", ttext (NE.head locTypePrefix) <> ":" <> ttext l ]
        , ""
        , renderLocationG l
        ]
      , ""
      ]
    | Example l e <- locTypeExamples
    ]
  ]
 where
  prefixList = encloseSep "(" ")" ", " (ttext <$> NE.toList locTypePrefix)
  renderLocationG l = case parseEither (parser locTypeGrammar ()) "LOCATION" l of
    Left msg -> string msg
    Right (locTypeFinder -> finder) -> case finderLocation finder of
      Left _ -> "(unpure)"
      Right (buildLocation finder -> Location {..}) -> vsep
        [ "fetches:" <+> ttext locUrl
        , "name:" <+> ttext locName
        , "do:" <+> D.string (show locMode)
        ]

-- | Parse a Location Finder
locationFinderP :: LocationType -> P LocationFinder
locationFinderP (LocationType UnpackedLocationType {..}) = do
  P.label "location type" $
    msum [ P.string' prfx | prfx <- NE.toList locTypePrefix ]
  P.char ':'
  locTypeFinder <$> parser locTypeGrammar ()

-- | Parse any of the locations
anyLocationFinderP :: [LocationType] -> P LocationFinder
anyLocationFinderP =
  msum . map locationFinderP

-- | Render a FetchUrl to Text.
renderLocation :: Location -> Sha256 -> Text
renderLocation Location {..} (sha256AsText -> sha256) =
  [Neat.text|${importStart}builtins.$fetcher {
    name   = "$locName";
    url    = "$locUrl";
    sha256 = "$sha256";
  }${importEnd}|]
 where
  importStart = if locMode >= Import then "import (" else ""
  importEnd   = if locMode >= Import then ")" else ""
  fetcher = if locMode >= Unpack then "fetchTarball" else "fetchurl"

-- | Prefetch a url, add it to the database and return a hash if it
-- succeeds.
prefetchIO :: Location -> IO (Maybe Sha256)
prefetchIO loc = do
  (ec, out) <- readProcessStdout . proc "nix-prefetch-url" $
    prefetchArgs loc
  return $ case ec of
    ExitSuccess   ->
      Just (Sha256 . LazyText.toStrict . LazyText.strip $ LazyText.decodeUtf8 out)
    ExitFailure _ ->
      Nothing
 where
  prefetchArgs Location {..} = Text.unpack <$> concat
    [ [ "--unpack" | locMode >= Unpack ]
    , [ "--name", locName ]
    , [ locUrl ]
    ]

-- | A simple wraper around Text.
newtype Sha256 = Sha256
  { sha256AsText :: Text
  } deriving (Show)

zeroSha256 :: Sha256
zeroSha256 = Sha256 "0000000000000000000000000000000000000000000000000000"

-- DiffText, simply do a diff of the previous text and the new, and add
-- colors.
diffText ::
  Bool
  -- ^ Use color?
  -> Text
  -- ^ Previous text
  -> Text
  -- ^ Future text
  -> Maybe String
diffText useColor from to = case getGroupedDiff (Text.lines from) (Text.lines to) of
  [] -> Nothing
  diff -> Just . unlines $ flip concatMap diff \case
    Both _ ss -> prefixAll "    " ss
    First  ss -> prefixAll (color Red   "--- ") ss
    Second ss -> prefixAll (color Green "+++ ") ss
 where
  prefixAll x ss = map ((x ++) . Text.unpack) ss
  color c x
    | useColor  = setSGRCode [SetColor Foreground Dull c] ++ x ++ setSGRCode [Reset]
    | otherwise = x

ttext :: Text -> Doc
ttext = D.text . Text.unpack

paragraph :: Text -> Doc
paragraph = D.fillSep . map ttext . Text.words
