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
import           Data.Version
import           Data.Foldable
import qualified Data.List.NonEmpty as NE
import qualified Data.List as L
import           Data.Maybe
import           System.Exit

-- text
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy                as LazyText
import qualified Data.Text.Lazy.IO             as LazyText
import qualified Data.Text.Lazy.Encoding       as LazyText
import           Data.Text.Lazy.Builder        as Builder (Builder, fromText, toLazyText) 
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

-- fixnix
import           Paths_fixnix

data LocationExample = LocationExample
  { exampleText :: Text
  , exampleHelp :: D.Doc
  }

example :: Text.Text -> D.Doc -> LocationExample
example = LocationExample

-- | A simple parser
type P = P.Parsec Void Text

data UrlFinder 
  = PureUrl (Text, Text)
  | UnpureUrl (IO (Text, Text))

-- | A location type
data LocationType a = LocationType
  { locName          :: Text
  , locPrefix        :: NE.NonEmpty Text
  , locDocumentation :: D.Doc
  , locExamples      :: [ LocationExample ]
  , locParser        :: P a
  , locPrinter       :: a -> Builder
  , locToUrl         :: a -> UrlFinder
  }

data AnyLocationType = forall a. (Show a, Eq a) => AnyLocationType (LocationType a)

data Location = forall a. Location (LocationType a) a

aLocationP :: LocationType a -> P a
aLocationP LocationType {..} = do
  P.label "location type" $ 
    msum [ P.string' prfx | prfx <- NE.toList locPrefix ]
  P.char ':'
  locParser

locationP :: AnyLocationType -> P Location
locationP (AnyLocationType lt) = 
  Location lt <$> aLocationP lt

oneOfLocationP :: [AnyLocationType] -> P Location
oneOfLocationP = msum . map locationP

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

parseALocation :: LocationType a -> Text -> Either String a
parseALocation lt = 
  first P.errorBundlePretty . P.parse (aLocationP lt)  "LOCATION" 

readLocation :: [AnyLocationType] -> ReadM Location
readLocation ltps = eitherReader 
  $ first P.errorBundlePretty . P.parse (oneOfLocationP ltps) "LOCATION" . Text.pack


parseAddConfig :: [AnyLocationType] -> Parser AddConfig
parseAddConfig ltps = do
  cfgLocation <- argument (readLocation ltps) $ 
    metavar "LOCATION"
    <> help "The location to download"

  cfgName <- optional . strOption $
    long "name"
    <> metavar "NAME"
    <> help "The name of the fix deriviation."
  
  cfgFixFile <- optional . argument (maybeReader parseRelFile) $
    metavar "FILE"
    <> help "The relative path to the fix file."
  
  cfgUnpack <- switch $
    long "unpack"
    <> short 'u'
    <> help "Should the item be unpacked? (tar only)"
    <> hidden

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
    <> header ("fixnix - a nix version fixer (version " ++ showVersion Paths_fixnix.version ++ ")")
    <> footerDoc (Just footer)
    )
 where
  footer = D.vcat
    [ "Below is a list of defined locations." D.</> "If the list is incomplete" D.</> "please" 
      D.</> "file a bug report to https://github.com/kalhauge/fixnix."
    , ""
    , D.vcat $ flip map ltps \(AnyLocationType (lt@LocationType {..})) -> D.nest 2 $ D.vcat
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
          , case parseALocation lt (NE.head locPrefix <> ":" <> l) of 
              Left msg -> D.string msg
              Right a -> case locToUrl a of
                UnpureUrl _ -> "(unpure)"
                PureUrl (url, name) -> D.vsep 
                  [ "fetches: " <> ttext url
                  , "name: " <> ttext name
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
  deriving (Show)

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
  let fetcher = case fetchUrlUnpack of 
        False -> "fetchurl"
        True  -> "fetchTarball"
  in [text|builtins.$fetcher {$name
       url    = "$fetchUrlUrl";
       sha256 = "$sha256";
     }|]
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

run :: IO ()
run = do
  AddConfig {..} <- readConfig locations
  case cfgLocation of
    Location lt a -> do
      (fetchUrlUrl, Just . flip fromMaybe cfgName -> fetchUrlName) <- 
        case locToUrl lt a of
          UnpureUrl a -> a
          PureUrl a -> pure a

      let fetchUrlUnpack = cfgUnpack
      let furl = FetchUrl {..}
      sha256 <- prefetchIO furl
      print sha256
      case sha256 of 
        Nothing -> fail "Could not prefetch url."
        Just sha256 -> do
          Text.putStrLn $ "# Auto-generated with fixnix (version " <> 
            (Text.pack $ showVersion Paths_fixnix.version) <> ")"
          LazyText.putStrLn . Builder.toLazyText $ "# location: " <> locPrinter lt a
          Text.putStrLn (renderFetchUrl furl sha256)

-- * Locations
locations :: [ AnyLocationType ]
locations = 
  [ AnyLocationType nixpkgsLocation
  , AnyLocationType githubLocation 
  ]


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
        "Accesss the tag of a github page."
    , example "nixos/nixpkgs/rev/1234abcd" 
        "Accesss the revision of a github page."
    , example "nixos/nixpkgs/heads/nixpkgs-20.03" 
        $ "Accesss the branch" D.</> "of a github page."
        D.</> "It will use" D.</> "`git ls-remote` to fix the current revision."
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
    case commit of 
      GitTag tag -> PureUrl 
        ( base <> "/archive/" <> tag <> ".tar.gz"
        , gitHubRepo <> "_" <> tag
        )
      GitRevision rev -> PureUrl 
        ( base <> "/archive/" <> rev <> ".tar.gz"
        , gitHubRepo <> "_" <> rev
        )
      GitBranch branch -> UnpureUrl $ do
        out <- readProcessStdout_
          $ proc "git" ["ls-remote", Text.unpack base, Text.unpack branch]
        case L.uncons . LazyText.words $ LazyText.decodeUtf8 out of
          Just (LazyText.toStrict -> rev, _) -> return
            ( base <> "/archive/" <> rev <> ".tar.gz"
            , gitHubRepo <> "_" <> branch <> "_" <> (Text.take 6 rev)
            )
          Nothing -> 
            fail $ "Could not find branch: " ++ Text.unpack branch
   where
    base = "https://github.com/" <> gitHubOwner <> "/" <> gitHubRepo

-- | Nixpkgs
nixpkgsLocation :: LocationType GitCommit
nixpkgsLocation = LocationType {..} where
  locName = "Nixpkgs"
  locPrefix = "nixpkgs" NE.:| ["nixos", "nix", "n"]
  locDocumentation =
    "Fix a version of the nixpkgs."
  locExamples = 
    [ example "heads/nixos-20.03" 
        "Use the 'nixos-20.03' branch."
    , example "heads/nixos-unstable"
        "Use the 'nixos-unstable' branch."
    -- , example "nixos/nixpkgs/rev/1234abcd" 
    --     "Accesss the revision of a github page."
    -- , example "nixos/nixpkgs/heads/nixpkgs-20.03" 
    --     $ "Accesss the branch" D.</> "of a github page."
    --     D.</> "It will use" D.</> "`git ls-remote` to fix the current revision."
    ]
  locParser = gitCommitP

  locPrinter commit = 
    case commit of
      GitBranch   branch -> "heads/" <> Builder.fromText branch
      GitTag      tag    -> "tags/"  <> Builder.fromText tag
      GitRevision rev    -> "rev/"  <> Builder.fromText rev

  locToUrl commit = do
    case commit of 
      GitTag tag -> PureUrl 
        ( base <> "/archive/" <> tag <> ".tar.gz"
        , gitHubRepo <> "_" <> tag
        )
      GitRevision rev -> PureUrl 
        ( base <> "/archive/" <> rev <> ".tar.gz"
        , gitHubRepo <> "_" <> rev
        )
      GitBranch branch -> UnpureUrl $ do
        out <- readProcessStdout_
          $ proc "git" ["ls-remote", Text.unpack base, Text.unpack branch]
        case L.uncons . LazyText.words $ LazyText.decodeUtf8 out of
          Just (LazyText.toStrict -> rev, _) -> return
            ( base <> "/archive/" <> rev <> ".tar.gz"
            , gitHubRepo <> "_" <> branch <> "_" <> (Text.take 6 rev)
            )
          Nothing -> 
            fail $ "Could not find branch: " ++ Text.unpack branch
   where
    gitHubOwner = "nixos"
    gitHubRepo = "nixpkgs"
    base = "https://github.com/" <> gitHubOwner <> "/" <> gitHubRepo

