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
import           Control.Applicative
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

-- fixnix
import FixNix.Core
import FixNix.Grammar

-- * Utils

data GitCommit
  = GitBranch    !Text
  | GitTag       !Text
  | GitRevision  !Text
  deriving (Show, Eq)
  --    | GitWildCard  !Text

$(makeCoLimit ''GitCommit)


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
        $ paragraph "Accesss the branch of a github page. It will use `git ls-remote` to fix the current revision."
    ]
  locTypeGrammar = defP $ Three
    (until1G "git-owner" '/')
    (until1G "git-repo" '/')
    gitCommitGrammar

  locTypeFinder a@(gitHubOwner, gitHubRepo, commit) = LocationFinder { .. }
   where
    finderBaseName = gitHubRepo
    finderIdentifier =
      case prettyText locTypeGrammar a of
        Right a   -> a
        Left  msg -> error $
          "Grammar for " <> Text.unpack locTypeName
          <> " is broken, please report!\n -  " <> msg

    finderLocationMode   = Unpack
    finderLocation = case commit of
      GitTag tag -> Right $ LocationBuilder
        { locBUrl = baseUrl <> "/archive/" <> tag <> ".tar.gz"
        , locBSuffix = Just tag
        }
      GitRevision rev -> Right $ LocationBuilder
        { locBUrl = baseUrl <> "/archive/" <> rev <> ".tar.gz"
        , locBSuffix = Just rev
        }
      GitBranch branch -> Left $ do
        out <- readProcessStdout_
          $ proc "git" ["ls-remote", Text.unpack baseUrl, Text.unpack branch]
        case L.uncons . LazyText.words $ LazyText.decodeUtf8 out of
          Just (LazyText.toStrict -> rev, _) -> return $ LocationBuilder
            { locBUrl = baseUrl <> "/archive/" <> rev <> ".tar.gz"
            , locBSuffix = Just $ branch <> "_" <> (Text.take 6 rev)
            }
          Nothing ->
            fail $ "Could not find branch: " ++ Text.unpack branch
     where
      baseUrl = "https://github.com/" <> gitHubOwner <> "/" <> gitHubRepo

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



gitCommitGrammar :: LocationG GitCommit
gitCommitGrammar = Group "git-commit" "a git commit" $ defS GitCommitCoLim
  { ifGitBranch = "heads/" **> restG "head"
  , ifGitTag = "tags/" **> restG "tag"
  , ifGitRevision = "rev/" **> restG "rev"
  }
