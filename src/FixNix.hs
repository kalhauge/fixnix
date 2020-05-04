{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module FixNix where

-- base 
import Data.Bifunctor
import Data.Void
import Data.Functor
import Control.Monad

-- text
import qualified Data.Text as Text

-- megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

-- optparse-applicative
import Options.Applicative

-- path
import Path

-- | The location of the tar-file.
data Location 
  = GitHub GitHubConfig GitCommit
  deriving (Show)

data GitCommit
  = GitBranch    !Text.Text
  | GitTag       !Text.Text
  | GitRevision  !Text.Text
  deriving (Show)

data GitHubConfig = GitHubConfig
  { gitHubOwner :: !Text.Text
  , gitHubRepo  :: !Text.Text
  }
  deriving (Show)

data AddConfig = AddConfig
  { cfgFixFile  :: !(Path Rel File)
  , cfgLocation :: !(Location)
  , cfgName     :: !(Maybe Text.Text)
  }
  deriving (Show)

readLocation :: ReadM Location
readLocation = eitherReader 
  $ first errorBundlePretty . parse locationP "LOCATION"

locationP :: Parsec Void String Location
locationP = do 
    locationParser <- label "location type" $ msum 
      [ string' "nixos" $> do 
        GitHub 
          (GitHubConfig { gitHubOwner = "NixOS", gitHubRepo = "nixpkgs" })
          <$> gitCommitP 
      , string' "github" $> do
        GitHub <$> (gitHubConfigP <* char '/') <*> gitCommitP
      ]
    char ':'
    l <- locationParser
    eof
    return l
 where
  gitCommitP = msum 
    [ GitTag      . Text.pack <$> (string' "tags/" *> takeWhileP Nothing (':' /=))
    , GitBranch   . Text.pack <$> (string' "heads/" *> takeWhileP Nothing (':' /=))
    , GitRevision . Text.toLower . Text.pack <$> Text.Megaparsec.many hexDigitChar
    ]

  gitHubConfigP = do
    gitHubOwner <- Text.pack <$>
      takeWhileP Nothing ('/' /=)

    char '/'

    gitHubRepo <- Text.pack <$>
      takeWhileP Nothing ('/' /=)

    pure $ GitHubConfig { gitHubOwner, gitHubRepo }

parseAddConfig :: Parser AddConfig
parseAddConfig = do
  cfgLocation <- argument readLocation $ 
    metavar "LOCATION"
    <> help "the location to download"

  cfgFixFile <- argument (maybeReader parseRelFile) $
    metavar "FILE"
    <> help "the relative path to the fix file."
  
  cfgName <- optional . strOption $
    long "name"
    <> metavar "NAME"
    <> help "the name of the file."

  pure $ AddConfig 
    { cfgFixFile
    , cfgLocation
    , cfgName
    }

readConfig :: IO AddConfig 
readConfig = execParser $ 
  info (parseAddConfig <**> helper)
    ( fullDesc
    <> header "fixnix - a nix version fixer"
    )

run :: IO ()
run = do
  cfg <- readConfig

  path


  print cfg

