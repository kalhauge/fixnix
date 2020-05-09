{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module FixNix where

-- base 
import Data.Bifunctor
import Data.Void
import Control.Monad
import System.Exit

-- containers
import qualified Data.Set as S

-- text
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
--import qualified Data.Text.IO as Text

-- megaparsec
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P

-- neat-interpolation
import NeatInterpolation (text)

-- optparse-applicative
import Options.Applicative

-- path
import Path

-- typed-process
import System.Process.Typed

type P = P.Parsec Void Text.Text

data LocationExample = LocationExample
  { exampleText :: Text.Text
  , exampleHelp :: Text.Text
  }

example :: Text.Text -> Text.Text -> LocationExample
example = LocationExample

-- | A location type
data LocationType = LocationType
  { locationName     :: Text.Text
  , locationPrefix   :: S.Set Text.Text
  , locationExamples :: [ LocationExample ]
  , locationParser   :: P (IO (Text.Text, Maybe Text.Text))
  }

githubLocation = LocationType
  { locationName = "GitHub"
  , locationPrefix = S.fromList [ "gh", "github" ]
  , locationExamples = 
    [ example "github:nixos/nixpkgs/tags/20.03" 
        "accesss the tag of a github page"
    , example "github:nixos/nixpkgs/heads/nixpkgs-20.03" 
        "accesss the branch of a github page"
    , example "gh:nixos/nixpkgs/rev/1234abcd" 
        "accesss the revision of a github page"
    ]
  , locationParser = do
      gitHubOwner <- P.takeWhile1P Nothing ('/' /=)

      P.char '/'

      gitHubRepo <- P.takeWhile1P Nothing ('/' /=)

      P.char '/'

      commit <- gitCommitP

      return $ do 
        case commit of
          GitRevision rev -> do
            return 
              ( [text|https://github.com/$gitHubOwner/$gitHubRepo/archive/$rev.tar.gz|]
              , Just [text|${gitHubRepo}_$txt|]
              )
          GitTag tag -> do
            return 
              ( [text|https://github.com/$gitHubOwner/$gitHubRepo/archive/$tag.tar.gz|]
              , Just [text|${gitHubRepo}_$txt|]
              )
          -- GitBranch bar -> do
  }

gitCommitP :: P GitCommit
gitCommitP = msum 
  [ GitTag      . Text.pack <$> do
    P.string' "tags/"  P.<?> "\"tags/\" for tags"
    P.someTill P.printChar P.eof P.<?> "a tag name"

  , GitBranch   . Text.pack <$> do
    P.string' "heads/" P.<?> "\"heads/\" for branches"
    P.someTill P.printChar P.eof P.<?> "a branch name"

  , GitRevision . Text.toLower . Text.pack <$> do
    P.string' "rev/" P.<?> "\"rev/\" for revisions"
    P.someTill P.hexDigitChar P.eof P.<?> "a revision"
  ]

data GitCommit
  = GitBranch    !Text.Text
  | GitTag       !Text.Text
  | GitRevision  !Text.Text
--  | GitWildCard  !Text.Text
  deriving (Show)

data AddConfig = AddConfig
  { cfgFixFile  :: !(Path Rel File)
  , cfgLocation :: !(LocationType, Text.Text, Maybe Text.Text)
  , cfgName     :: !(Maybe Text.Text)
  }
--   deriving (Show)

readLocation :: [LocationType] -> ReadM (LocationType, Text.Text, Maybe Text.Text)
readLocation ltps = eitherReader 
  $ first P.errorBundlePretty . P.parse (locationP ltps) "LOCATION" . Text.pack

locationP :: [LocationType] -> P (LocationType, Text.Text, Maybe Text.Text)
locationP ltps = msum 
    [ do
      P.label "location type" $ 
        msum [ P.string' prfx | prfx <- S.toList locationPrefix ]
      P.char ':'
      (url, name) <- locationParser
      return (lt, url, name)
    | lt@(LocationType {..}) <- ltps
    ]
 where

parseAddConfig :: [LocationType] -> Parser AddConfig
parseAddConfig ltps = do
  cfgLocation <- argument (readLocation ltps) $ 
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

readConfig :: [LocationType] -> IO AddConfig 
readConfig ltps = execParser $ 
  info (parseAddConfig ltps <**> helper)
    ( fullDesc
    <> header "fixnix - a nix version fixer"
    )

newtype Sha256 = Sha256 
  { sha256AsText :: Text.Text }

zeroSha256 :: Sha256
zeroSha256 = 
  Sha256 "0000000000000000000000000000000000000000000000000000"

data FetchUrl = FetchUrl
  { fetchUrlUrl       :: Text.Text
  , fetchUrlName      :: Maybe Text.Text
  , fetchUrlUnpack    :: Bool
  } 
  deriving (Show)

renderFetchUrl :: FetchUrl -> Sha256 -> Text.Text
renderFetchUrl FetchUrl {..} (sha256AsText -> sha256) =
  case fetchUrlUnpack of 
    True -> [text|builtins.fetchurl {$name
              url    = "$fetchUrlUrl";
              sha256 = "$sha256";
            }|]
    False -> ""
 where
  name = case fetchUrlName of 
    Just n -> "\n  " <> [text|name   = "$n";|] 
    Nothing -> ""

prefetchArgs :: FetchUrl -> [ String ]
prefetchArgs FetchUrl {..} = Text.unpack <$> concat 
  [ [ "--unpack" | fetchUrlUnpack ]
  , maybe [] (\name -> [ "--name", name ]) fetchUrlName
  , [ fetchUrlUrl ]
  ]

prefetchIO :: FetchUrl -> IO (Maybe Sha256)
prefetchIO furl = do
  (ec, out) <- readProcessStdout . proc "nix-prefetch-url" $
    prefetchArgs furl
  return $ case ec of 
    ExitSuccess   -> Just (Sha256 . LazyText.toStrict $ LazyText.decodeUtf8 out)
    ExitFailure _ -> Nothing

locations = [ githubLocation ]

run :: IO ()
run = do
  cfg <- readConfig locations
  return ()
  -- case cfgLocation cfg of 
  --   GitHub gf gc  -> FetchUrl 
  --     { fetchUrlUrl :: [text|http://github.com/$|]
  --     , fetchUrlName = 
  --     }
  -- prefetchIO

