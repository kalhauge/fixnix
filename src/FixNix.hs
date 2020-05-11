{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module FixNix where

-- base 
import           Data.Bifunctor
import           Data.Void
import           Data.Foldable
import qualified Data.List.NonEmpty as NE
import qualified Data.List as L
import           Data.Maybe
import           System.Exit

-- text
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy                as LazyText
-- import qualified Data.Text.Lazy.IO             as LazyText
import qualified Data.Text.Lazy.Encoding       as LazyText
import           Data.Text.Lazy.Builder        as Builder (Builder, fromText) 
import qualified Data.Text.IO as Text

-- megaparsec
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P

-- neat-interpolation
import           NeatInterpolation              ( text )

-- optparse-applicative
import           Options.Applicative
import qualified Options.Applicative.Help.Pretty as D

-- path
import           Path

-- typed-process
import           System.Process.Typed


data LocationExample = LocationExample
  { exampleText :: Text
  , exampleHelp :: D.Doc
  }

example :: Text.Text -> D.Doc -> LocationExample
example = LocationExample

-- | A simple parser
type P = P.Parsec Void Text

-- | A location type
data LocationType a = LocationType
  { locName          :: Text
  , locPrefix        :: NE.NonEmpty Text
  , locDocumentation :: D.Doc
  , locExamples      :: [ LocationExample ]
  , locParser        :: P a
  , locPrinter       :: a -> Builder
  , locToUrl         :: a -> IO (Text, Text)
  }

data AnyLocationType = forall a. (Show a, Eq a) => AnyLocationType (LocationType a)

data Location = forall a. Location (LocationType a) a

locationP :: AnyLocationType -> P Location
locationP (AnyLocationType (lt@(LocationType {..}))) = do
  P.label "location type" $ 
    msum [ P.string' prfx | prfx <- NE.toList locPrefix ]
  P.char ':'
  Location lt <$> locParser

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


data AddConfig = AddConfig
  { cfgFixFile  :: !(Maybe (Path Rel File))
  , cfgLocation :: !Location
  , cfgName     :: !(Maybe Text.Text)
  , cfgUnpack   :: !Bool
  }
--   deriving (Show)

readLocation :: [AnyLocationType] -> ReadM Location
readLocation ltps = eitherReader 
  $ first P.errorBundlePretty . P.parse (oneOfLocationP ltps) "LOCATION" . Text.pack

oneOfLocationP :: [AnyLocationType] -> P Location
oneOfLocationP = msum . map locationP

parseAddConfig :: [AnyLocationType] -> Parser AddConfig
parseAddConfig ltps = do
  cfgLocation <- argument (readLocation ltps) $ 
    metavar "LOCATION"
    <> help "The location to download"

  cfgFixFile <- optional . argument (maybeReader parseRelFile) $
    metavar "FILE"
    <> help "The relative path to the fix file."

  cfgName <- optional . strOption $
    long "name"
    <> metavar "NAME"
    <> help "The name of the fix deriviation."
  
  cfgUnpack <- switch $
    long "unpack"
    <> short 'u'
    <> help "Should the item be unpacked?"

  pure $ AddConfig 
    { cfgFixFile
    , cfgLocation
    , cfgName
    , cfgUnpack
    }

readConfig :: [AnyLocationType] -> IO AddConfig 
readConfig ltps = execParser $ 
  info (parseAddConfig ltps <**> helper)
    ( fullDesc
    <> header "fixnix - a nix version fixer"
    <> footerDoc (Just footer)
    )
 where
  footer = D.vcat
    [ "Doc string"
    , ""
    , flip foldMap ltps \(AnyLocationType (LocationType {..})) -> D.nest 2 $ D.vcat
      [ D.hsep 
        [ "*", D.text (Text.unpack locName)
        , D.encloseSep "(" ")" ", " (D.text . Text.unpack <$> NE.toList locPrefix)
        ]
      , ""
      , locDocumentation
      , ""
      , "Examples:"
      , ""
      , D.vsep $ 
        [ D.nest 2 . D.vsep $
          [ D.hsep 
            [ "$"
            , "fixnix"
            , ttext (NE.head locPrefix) <> ":" <> ttext l
            ]
          , ""
          , e
          , ""
          ]
        | LocationExample l e <- locExamples
        ]
      ]
    ]

ttext = D.text . Text.unpack

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

locations :: [ AnyLocationType ]
locations = 
  [ AnyLocationType githubLocation 
  ]

run :: IO ()
run = do
  AddConfig {..} <- readConfig locations
  case cfgLocation of
    Location lt a -> do
      (fetchUrlUrl, Just . flip fromMaybe cfgName -> fetchUrlName) <- locToUrl lt a
      let fetchUrlUnpack = cfgUnpack
      let furl = FetchUrl {..}
      sha256 <- prefetchIO furl
      case sha256 of 
        Nothing -> fail "Could not prefetch url."
        Just sha256 -> 
          Text.putStrLn (renderFetchUrl furl sha256)
-- * Locations

data GitCommit
  = GitBranch    !Text
  | GitTag       !Text
  | GitRevision  !Text
  | GitWildCard  !Text
  deriving (Show, Eq)

-- | GitHub have an intersting API for connecting and downloading branches
-- and tags.
githubLocation :: LocationType (Text, Text, GitCommit)
githubLocation = LocationType {..} where
  locName = "GitHub"
  locPrefix = "github" NE.:| ["gh"]
  locDocumentation =
    "Connect to the github API."
  locExamples = 
    [ example "nixos/nixpkgs/tags/20.03" 
        "Accesss the tag of a github page"
    , example "nixos/nixpkgs/heads/nixpkgs-20.03" 
        $ "Accesss the branch" D.</> "of a github page."
        D.</> "It will use" D.</> "`git ls-remote` to fix the current revision."
    , example "nixos/nixpkgs/rev/1234abcd" 
        "Accesss the revision of a github page"
    ]
  locParser = do
    gitHubOwner <- P.takeWhile1P Nothing ('/' /=)
    P.char '/'

    gitHubRepo <- P.takeWhile1P Nothing ('/' /=)
    P.char '/'

    (gitHubOwner, gitHubRepo,) <$> gitCommitP

  locPrinter (gitHubOwner, gitHubRepo, commit) = 
    Builder.fromText gitHubOwner <> "/" 
    <> Builder.fromText gitHubRepo <> "/" 
    <> case commit of
        GitBranch   branch -> "heads/" <> Builder.fromText branch
        GitTag      tag    -> "tags/"  <> Builder.fromText tag
        GitRevision rev    -> "rev/"  <> Builder.fromText rev

  locToUrl (gitHubOwner, gitHubRepo, commit) = do
    let
      base = "https://github.com/" <> gitHubOwner <> "/" <> gitHubRepo

    (url, name) <- case commit of 
      GitTag tag -> return 
        ( tag <> ".tar.gz"
        , gitHubRepo <> "_" <> tag
        )
      GitRevision rev -> return 
        ( rev <> ".tar.gz"
        , gitHubRepo <> "_" <> rev
        )
      GitBranch branch -> do
        out <- readProcessStdout_
          $ proc "git" ["ls-remote", Text.unpack base, Text.unpack branch]
        case L.uncons . LazyText.words $ LazyText.decodeUtf8 out of
          Just (LazyText.toStrict -> rev, _) -> return
            ( rev <> ".tar.gz"
            , gitHubRepo <> "_" <> branch <> "_" <> (Text.take 6 rev)
            )
          Nothing -> 
            fail $ "Could not find branch: " ++ Text.unpack branch

    return 
      ( base <> "/archive/" <> url
      , name
      )



