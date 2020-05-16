{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      :  FixNix.Locations
-- Copyright   :  (c) Christian Gram Kalhauge 2020
-- License     :  BSD3
-- Maintainer  :  christian@kalhauge.dk
--
module FixNix.Locations where

-- base 
import           Data.Foldable
import qualified Data.List.NonEmpty as NE
import qualified Data.List as L

-- text
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy                as LazyText
import qualified Data.Text.Lazy.Encoding       as LazyText

-- megaparsec
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P

-- optparse-applicative
import           Options.Applicative.Help.Pretty as D hiding (text)

-- typed-process
import           System.Process.Typed

-- fixnix
import FixNix.Core

-- * Locations
locations :: [ LocationType ]
locations = 
  [ githubLocation 
  ]

-- | GitHub have an intersting API for connecting and downloading branches
-- and tags.
githubLocation :: LocationType 
githubLocation = LocationType {..} where
  locTypeName = "GitHub"
  locTypePrefix = "github" NE.:| ["gh"]
  locTypeDocumentation =
    "Connect to the github API."
  locTypeExamples = 
    [ Example "nixos/nixpkgs/tags/20.03" 
        "Accesss the tag of a github page."
    , Example "nixos/nixpkgs/rev/1234abcd" 
        "Accesss the revision of a github page."
    , Example "nixos/nixpkgs/heads/nixpkgs-20.03" 
        $ "Accesss the branch" D.</> "of a github page."
        D.</> "It will use" D.</> "`git ls-remote` to fix the current revision."
    ]
  locTypeParser = do
    gitHubOwner <- P.takeWhile1P Nothing ('/' /=)
    P.char '/'

    gitHubRepo <- P.takeWhile1P Nothing ('/' /=)
    P.char '/'

    commit <- gitCommitP

    return (gitHubOwner, gitHubRepo, commit)

  locTypeFinder (gitHubOwner, gitHubRepo, commit) = LocationFinder 
    (Text.intercalate "/" [ gitHubOwner, gitHubRepo, gitCommitToText commit ])
    base
    $ case commit of 
      GitTag tag -> Right $ Location
        { locUrl = baseUrl <> "/archive/" <> tag <> ".tar.gz"
        , locName = base <> "_" <> tag
        }
      GitRevision rev -> Right $ Location
        { locUrl = baseUrl <> "/archive/" <> rev <> ".tar.gz"
        , locName = base <> "_" <> rev
        }
      GitBranch branch -> Left $ do
        out <- readProcessStdout_
          $ proc "git" ["ls-remote", Text.unpack base, Text.unpack branch]
        case L.uncons . LazyText.words $ LazyText.decodeUtf8 out of
          Just (LazyText.toStrict -> rev, _) -> return $ Location
            { locUrl = baseUrl <> "/archive/" <> rev <> ".tar.gz"
            , locName = base <> "_" <> branch <> "_" <> (Text.take 6 rev)
            }
          Nothing -> 
            fail $ "Could not find branch: " ++ Text.unpack branch
     where
      baseUrl = "https://github.com/" <> gitHubOwner <> "/" <> gitHubRepo
      base = gitHubRepo

-- -- | Nixpkgs
-- nixpkgsLocation :: LocationType 
-- nixpkgsLocation = LocationType {..} where
--   locName = "Nixpkgs"
--   locPrefix = "nixpkgs" NE.:| ["nixos", "nix", "n"]
--   locDocumentation =
--     "Fix a version of the nixpkgs."
--   locExamples = 
--     [ Example "heads/nixos-20.03" 
--         "Use the 'nixos-20.03' branch."
--     , Example "heads/nixos-unstable"
--         "Use the 'nixos-unstable' branch."
--     ]
--   locParser = gitCommitP
-- 
--   locPrinter commit = 
--     case commit of
--       GitBranch   branch -> "heads/" <> Builder.fromText branch
--       GitTag      tag    -> "tags/"  <> Builder.fromText tag
--       GitRevision rev    -> "rev/"  <> Builder.fromText rev
-- 
--   locToUrl commit = do
--     case commit of 
--       GitTag tag -> PureUrl 
--         ( base <> "/archive/" <> tag <> ".tar.gz"
--         , gitHubRepo <> "_" <> tag
--         )
--       GitRevision rev -> PureUrl 
--         ( base <> "/archive/" <> rev <> ".tar.gz"
--         , gitHubRepo <> "_" <> rev
--         )
--       GitBranch branch -> UnpureUrl $ do
--         out <- readProcessStdout_
--           $ proc "git" ["ls-remote", Text.unpack base, Text.unpack branch]
--         case L.uncons . LazyText.words $ LazyText.decodeUtf8 out of
--           Just (LazyText.toStrict -> rev, _) -> return
--             ( base <> "/archive/" <> rev <> ".tar.gz"
--             , gitHubRepo <> "_" <> branch <> "_" <> (Text.take 6 rev)
--             )
--           Nothing -> 
--             fail $ "Could not find branch: " ++ Text.unpack branch
--    where
--     gitHubOwner = "nixos"
--     gitHubRepo = "nixpkgs"
--     base = "https://github.com/" <> gitHubOwner <> "/" <> gitHubRepo



-- * Utils

data GitCommit
  = GitBranch    !Text
  | GitTag       !Text
  | GitRevision  !Text
  | GitWildCard  !Text
  deriving (Show, Eq)

gitCommitP :: P GitCommit
gitCommitP = msum 
  [ GitTag      . Text.pack <$> do
    _ <- P.string' "tags/"  P.<?> "\"tags/\" for tags"
    P.someTill P.printChar P.eof P.<?> "a tag name"

  , GitBranch   . Text.pack <$> do
    _ <- P.string' "heads/" P.<?> "\"heads/\" for branches"
    P.someTill P.printChar P.eof P.<?> "a branch name"

  , GitRevision . Text.toLower . Text.pack <$> do
    _ <- P.string' "rev/" P.<?> "\"rev/\" for revisions"
    P.someTill P.hexDigitChar P.eof P.<?> "a revision"
  ]

gitCommitToText :: GitCommit -> Text
gitCommitToText = \case
  GitTag t      -> "tags/" <> t
  GitBranch b   -> "heads/" <> b
  GitRevision r -> "rev/" <> r


