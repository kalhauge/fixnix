{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
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
import           Control.Applicative hiding ((<**>))
import           Data.Functor.Contravariant
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


-- grammar
import Control.Grammar
import Control.Grammar.TH

-- directory
import System.Directory

-- path
import Path

-- fixnix
import FixNix.Core
import FixNix.Grammar

-- * Utils

data GitHubRepo = GitHubRepo
  { gitHubOwner :: !Text
  , gitHubRepo  :: !Text
  } deriving (Show, Eq)

$(makeLimit ''GitHubRepo)

data GitCommit
  = GitTag       !Text
  | GitRevision  !Text
  | GitMagic     !(Maybe Text)
  deriving (Show, Eq)
  --    | GitWildCard  !Text

$(makeCoLimit ''GitCommit)

gitHubRepoGrammar = defP $ GitHubRepoLim
  { onGitHubOwner = until1G "git-owner" '/' <** "/"
  , onGitHubRepo  = until1G "git-repo" '/'
  }

gitCommitGrammar :: LocationG GitCommit
gitCommitGrammar = Group "git-commit" "a git commit" $ defS GitCommitCoLim
  { ifGitTag = "/tags/" **> restG "tag"
  , ifGitRevision = "/rev/" **> restG "rev"
  , ifGitMagic = maybeG ("/" **> restG "magic") endG
  }

gitHubBaseUrl GitHubRepo {..} =
  "https://github.com/" <> gitHubOwner <> "/" <> gitHubRepo

githubLocationFinder :: GitHubRepo -> GitCommit -> Either (IO LocationBuilder) LocationBuilder
githubLocationFinder ghr = \case
  GitTag tag -> Right $ LocationBuilder
    { locBUrl = baseUrl <> "/archive/" <> tag <> ".tar.gz"
    , locBSuffix = Just tag
    }
  GitRevision rev -> Right $ LocationBuilder
    { locBUrl = baseUrl <> "/archive/" <> rev <> ".tar.gz"
    , locBSuffix = Just rev
    }
  GitMagic magic -> Left $ do
    out <- readProcessStdout_
      $ proc "git" ["ls-remote", Text.unpack baseUrl, maybe "HEAD" Text.unpack magic]
    case L.uncons . LazyText.words $ LazyText.decodeUtf8 out of
      Just (LazyText.toStrict -> rev, _) -> return $ LocationBuilder
        { locBUrl = baseUrl <> "/archive/" <> rev <> ".tar.gz"
        , locBSuffix = Just $
          foldMap (\m -> cleanPackageName m <> "_") magic <> (Text.take 6 rev)
        }
      Nothing ->
        fail $ "Could not find item: " ++ maybe "HEAD" Text.unpack magic
 where
  baseUrl = gitHubBaseUrl ghr
  cleanPackageName = Text.map (\c -> if c `L.elem` ("/" :: String) then '+' else c)

data Nixpkgs
  = NixUnstable
  | NixVersion   !Text
  deriving (Show, Eq)

$(makeCoLimit ''Nixpkgs)

-- * Locations
locations :: [ LocationType ]
locations =
  [ githubLocation
  , hackageLocation
  , nixpkgsLocation
  ]

-- | GitHub have an intersting API for connecting and downloading branches
-- and tags.
githubLocation :: LocationType
githubLocation = LocationType UnpackedLocationType {..} where
  locTypeName = "GitHub"
  locTypePrefix = "github" NE.:| ["gh"]
  locTypeDocumentation =
    "Connect to the github API."
  locTypeExamples =
    [ Example "nixos/nixpkgs/tags/20.03"
        "Accesss the tag of a github page."
    , Example "nixos/nixpkgs"
        "Accesss the HEAD of a github page."
    , Example "nixos/nixpkgs/topic"
        "Accesss the match of 'topic' to git ls-remote, fails if there are multiple matches."
    , Example "nixos/nixpkgs/rev/1234abcd"
        "Accesss the revision of a github page."
    , Example "nixos/nixpkgs/heads/nixpkgs-20.03"
        $ paragraph "Accesss the branch of a github page. It will use `git ls-remote` to fix the current revision."
    ]
  locTypeGrammar = gitHubRepoGrammar <**> gitCommitGrammar
  locTypeCompleter = \_ s -> return [s]
  locTypeFinder (gh@GitHubRepo {..}, commit) = LocationFinder { .. }
   where
    finderBaseName = gitHubRepo
    finderLocationMode   = Unpack
    finderLocation = githubLocationFinder gh commit

hackageLocation :: LocationType
hackageLocation = LocationType UnpackedLocationType {..} where
  locTypeName = "Hackage"
  locTypePrefix = "hackage" NE.:| ["h"]
  locTypeDocumentation =
    "Connect to the hackage server."
  locTypeExamples =
    [ Example "hspec-hedgehog/0.0.1.2"
        "Access a package of a specific version "
    ]
  locTypeGrammar = defP $ Two
    (until1IncG "package-name" '/')
    (restG "package-version")

  locTypeCompleter = \_ s -> do
    -- let f = Path.fromAbsDir (cfgCache cfg)
    -- createDirectoryIfMissing True f
    -- out <- readProcessStdout_
    --   $ proc "git" ["ls-remote", Text.unpack baseUrl, maybe "HEAD" Text.unpack magic]

    return [s]

  locTypeFinder (name, version) = LocationFinder { .. }
   where
    finderBaseName = name
    finderLocationMode   = Unpack
    finderLocation = return $ LocationBuilder
      { locBUrl = "https://hackage.haskell.org/package/" <> packagename <> "/" <>
        packagename <> ".tar.gz"
      , locBSuffix = Just version
      }
     where
      packagename = name <> "-" <> version


nixpkgsLocation :: LocationType
nixpkgsLocation = LocationType UnpackedLocationType {..} where
  locTypeName = "Nixpkgs"
  locTypePrefix = "nixpkgs" NE.:| ["nix", "n"]
  locTypeDocumentation =
    "Download a nix-channel."
  locTypeExamples =
    [ Example "20.03"
        "Download version 20.03"
    , Example "unstable"
        "Download unstable version"
    ]
  locTypeGrammar = defS $ NixpkgsCoLim
    "unstable"
    (restG "nixpkgs-version")

  locTypeCompleter = \_ s -> do
    -- let f = Path.fromAbsDir (cfgCache cfg)
    -- createDirectoryIfMissing True f
    -- out <- readProcessStdout_
    --   $ proc "git" ["ls-remote", Text.unpack baseUrl, maybe "HEAD" Text.unpack magic]

    return [s]

  locTypeFinder np = LocationFinder { .. }
   where
    finderBaseName = "nixpkgs"
    finderLocationMode = Import
    finderLocation = githubLocationFinder (GitHubRepo "nixos" "nixpkgs") $ case np of
      NixUnstable -> GitMagic . Just $ "heads/nixos-unstable"
      NixVersion v -> GitMagic . Just $ "heads/nixos-" <> v



