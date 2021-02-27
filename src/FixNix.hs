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
-- |
-- Module      :  FixNix
-- Copyright   :  (c) Christian Gram Kalhauge 2020
-- License     :  BSD3
-- Maintainer  :  christian@kalhauge.dk
--
-- FixNix is a small tool for fixing versions of nixpkgs and other
-- sources used when writing nix-expressions.
module FixNix where

-- base
import           Data.Version
import           Data.Maybe
import           Data.Function
import           Data.Foldable
import           Data.Functor
import qualified Data.List.NonEmpty as NE
import           Control.Monad
import           System.IO
import           System.IO.Error
import           System.Environment

-- directory
import System.Directory

-- text
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO as Text

-- optparse-applicative
import           Options.Applicative
import qualified Options.Applicative.Help.Pretty as D

-- megaparsec
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P

-- path
import           Path

-- fixnix
import           Paths_fixnix
import           FixNix.Core
import           FixNix.Grammar
import           FixNix.Locations

data GlobalConfig = GlobalConfig
  { cfgLocationTypes :: ![LocationType]
  , cfgCache :: !(Path Abs Dir)
  , cfgHistory :: !(Path Abs File)
  }

cfgFinderGrammar :: GlobalConfig -> LocationG TypedLocationFinder
cfgFinderGrammar cfg = finderG (cfgLocationTypes cfg)

mkGlobalConfig :: IO GlobalConfig
mkGlobalConfig = do
  cache <- lookupEnv "XDG_CACHE_HOME" >>= \case
    Just ch  -> parseAbsDir ch
    Nothing -> lookupEnv "HOME" >>= \case
      Just h -> (Path.</> [reldir|.config|]) <$> parseAbsDir h
      Nothing -> fail "Neither XDG_CACHE_HOME or HOME set."

  let
    cfgCache = cache </> [reldir|fixnix|]
    cfgLocationTypes = locations
    cfgHistory = cfgCache </> [relfile|history.txt|]

  tryIOError $
    createDirectoryIfMissing True (fromAbsDir cfgCache)

  return $ GlobalConfig { .. }

data Command
  = Print TypedLocationFinder
  | Add [TypedLocationFinder]
  | ListLocations

readLocationFinder :: LocationG TypedLocationFinder -> ReadM TypedLocationFinder
readLocationFinder grm = eitherReader $
  parseEither (parser grm ()) "LOCATION" . Text.pack

argLocation :: GlobalConfig -> Parser TypedLocationFinder
argLocation cfg = argument (readLocationFinder (cfgFinderGrammar cfg)) $ fold
  [ metavar "LOCATION"
  , help "The location to download (see 'fixnix list')"
  , completer . mkCompleter $ \(Text.pack -> s) -> do
      history <- readHistory cfg
      return . map Text.unpack . filter (Text.isPrefixOf s) $ history
  ]

    -- let
    --   prefixes = map ((<> ":") . Text.unpack . NE.head . locTypePrefix) ls
    --   valid = filter (\x -> s `isPrefix  )
    -- return
    -- [ a ++ x
    -- - | a <- map ((<>":") . ) ls
    -- , x <- [ "1", "2" ]
    -- ]
printCommand :: GlobalConfig -> Mod CommandFields Command
printCommand cfg = command "print" $
  info p (progDesc "print the fix-file to stdout")
 where
  p = do
    x <- argLocation cfg
    pure $ Print x

addCommand :: GlobalConfig -> Mod CommandFields Command
addCommand cfg = command "add" $
  info p (progDesc "add/override fix-file(s) to the fix-folder")
 where
  p = do
    x <- many $ argLocation cfg
    pure $ Add x

listCommand :: GlobalConfig -> Mod CommandFields Command
listCommand _ = command "list" $
  info (pure ListLocations) (progDesc "list the locations and their formats")

parseConfig :: GlobalConfig -> Parser Config
parseConfig _ = do
  cfgFixFolder <- option (maybeReader parseRelDir) $ fold
    [ short 'o'
    , long "fix-folder"
    , hidden
    , value [reldir|nix/fix|]
    , metavar "FOLDER"
    , action "directory"
    , showDefault
    , help "The relative path to the fix folder."
    ]

  cfgForce <- switch $ fold
    [ short 'f'
    , long "force"
    , hidden
    , help "Don't ask anoying questions, just do!"
    ]

  pure $ Config { .. }

fixnixParserInfo :: GlobalConfig -> ParserInfo (Config, Command)
fixnixParserInfo cfg = info
  (((,)
    <$> parseConfig cfg
    <*> hsubparser (printCommand cfg <> addCommand cfg <> listCommand cfg)
    ) <**> helper)
  $ fold
  [ fullDesc
  , header ("fixnix - a nix expression fixer (version " ++ showVersion Paths_fixnix.version ++ ")")
  , footerDoc (Just mempty)
  ]

listLocations :: [LocationType] -> Text
listLocations ltps = Text.pack . flip D.displayS "" . D.renderPretty 0.9 80 $ D.vsep
  [ "# Locations"
  , "A location is parsed like this:"
  , ""
  , D.indent 4 $ explainGrammar (finderG ltps)
  , ""
  , "Below is a list of defined locations." D.</> "If the list is incomplete" D.</> "please"
    D.</> "file a bug report to https://github.com/kalhauge/fixnix."
  , ""
  , D.vcat $ map describeLocationType ltps
  ]

fetchFixText :: GlobalConfig -> TypedLocationFinder -> IO Text
fetchFixText gcfg finder = do
  loc <- findLocation (finderLocationFinder finder)
  sha256 <- prefetchIO loc
  case sha256 of
    Nothing -> fail "Could not prefetch url."
    Just sha256' -> do
      txt <- case prettyText (cfgFinderGrammar gcfg) finder of
        Right txt -> return txt
        Left msg -> error ("Unexpected problem with location finder: " ++ msg)

      appendHistory gcfg txt

      args <- getArgs
      return $ renderFixNixExpr args loc sha256'
 where
  renderFixNixExpr args loc sha256 = foldMap (<> "\n")
    [ "# Auto-generated with fixnix (version " <> textVersion <> ")"
    , "# " <> Text.unwords (map Text.pack args)
    , renderLocation loc sha256
    ]
   where
    textVersion = Text.pack $ showVersion Paths_fixnix.version


appendHistory :: GlobalConfig -> Text -> IO ()
appendHistory cfg txt =
  Text.appendFile (fromAbsFile (cfgHistory cfg)) (txt <> "\n")

readHistory :: GlobalConfig -> IO [Text]
readHistory cfg =
  tryIOError (Text.readFile . fromAbsFile $ cfgHistory cfg) <&> \case
    Left _ -> []
    Right txt -> Text.lines txt

-- | Run the io
run :: IO ()
run = do
  globalcfg <- mkGlobalConfig
  (cfg, cmd) <- execParser
    . fixnixParserInfo
    $ globalcfg

  runWithConfig globalcfg cfg cmd

runWithConfig :: GlobalConfig -> Config -> Command -> IO ()
runWithConfig globalcfg cfg cmd =
  case cmd of
    Print x -> do
      txt <- fetchFixText globalcfg x
      Text.putStr txt
    Add xs -> forM_ xs \x -> do
      txt <- fetchFixText globalcfg x
      writeDiff txt x
    ListLocations ->
      Text.putStr $ listLocations locations

 where
  writeDiff txt loc = do
    let
      file = cfgFilename cfg (finderLocationFinder loc)
      filef = fromRelFile file
    createDirectoryIfMissing True (fromRelDir $ parent file)
    tryIOError (Text.readFile filef) >>= \case
      Left _ -> do
        hPutStrLn stderr "A new file."
        Text.writeFile filef txt
      Right txt2 -> case diffText True txt2 txt of
        Just msg -> do
          hPutStrLn stderr ("Found a file in " <> show file)
          hPutStr stderr msg
          confirm "Can I override this file?" do
            Text.writeFile filef txt
        Nothing  ->
          return ()

  confirm :: String -> IO () -> IO ()
  confirm help' dothis
    | cfgForce cfg = dothis
    | otherwise = do
      hPutStr stderr (help' ++ " [yes/no]: ")
      fix $ \rec -> do
        answer <- Text.strip . Text.toLower <$> Text.getLine
        case answer of
          "yes" -> dothis
          "no" -> return ()
          _ -> hPutStr stderr "Please answer yes or no." *> rec
      hPutStrLn stderr ""



