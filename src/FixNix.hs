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
import           Data.Functor
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

data Config = Config
  { cfgFixFolder      :: !(Path Rel Dir)
  }

cfgFilename :: Config -> LocationFinder -> Path Rel File
cfgFilename Config {..} lc@LocationFinder {..} = 
  case parseRelFile filename of
   Just rf -> cfgFixFolder </> rf
   Nothing -> error ("Bad base name " ++ filename)
 where 
  filename = Text.unpack $ finderBaseName <> ".nix"

data Command 
  = Print LocationFinder
  | Add   [LocationFinder]
  | ListLocations

readLocationFinder :: Grammar LocationFinder -> ReadM LocationFinder
readLocationFinder grm = eitherReader $ 
  parseEither (parser grm) "LOCATION" . Text.pack

argLocation :: Grammar LocationFinder -> Parser LocationFinder
argLocation grm = argument (readLocationFinder grm) $ 
  metavar "LOCATION"
  <> help "The location to download (see 'fixnix list)"

printCommand :: Grammar LocationFinder -> Mod CommandFields Command
printCommand grm = command "print" $
  info p (progDesc "print the fix-file to stdout")
 where
  p = do
    x <- argLocation grm
    pure $ Print x

addCommand :: Grammar LocationFinder -> Mod CommandFields Command
addCommand grm = command "add" $
  info p (progDesc "add/override fix-file(s) to the fix-folder")
 where
  p = do
    x <- many $ argLocation grm
    pure $ Add x

listCommand :: [LocationType] -> Mod CommandFields Command
listCommand ltps = command "list" $
  info (pure $ ListLocations) (progDesc "list the locations and their formats")

parseConfig :: Parser Config
parseConfig = do
  cfgFixFolder <- option (maybeReader parseRelDir) $
    short 'o'
    <> long "fix-folder"
    <> hidden
    <> value [reldir|nix/fix|]
    <> metavar "FOLDER"
    <> showDefault
    <> help "The relative path to the fix folder."

  pure $ Config 
    { cfgFixFolder
    }

fixnixParserInfo :: [LocationType] -> ParserInfo (Config, Command)
fixnixParserInfo ltps = info 
    (((,) <$> parseConfig <*> 
      hsubparser (printCommand grm <> addCommand grm <> listCommand ltps)
    ) <**> 
    helper) $
  fullDesc
  <> header ("fixnix - a nix expression fixer (version " ++ showVersion Paths_fixnix.version ++ ")")
  <> footerDoc (Just mempty)

 where grm = finderG ltps

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

fetchFixText :: LocationFinder -> IO Text
fetchFixText finder = do
  loc <- findLocation finder
  sha256 <- prefetchIO loc
  case sha256 of 
    Nothing -> fail "Could not prefetch url."
    Just sha256 -> do 
      args <- getArgs
      return $ renderFixNixExpr args loc sha256
 where 
  renderFixNixExpr args loc sha256 = foldMap (<> "\n")
    [ "# Auto-generated with fixnix (version " <> textVersion <> ")"
    , "# " <> Text.unwords (map Text.pack args)
    , renderLocation loc sha256
    ]
   where
    textVersion = Text.pack $ showVersion Paths_fixnix.version

-- | Run the io
run :: IO ()
run = do
  (cfg@Config {..}, cmd) <- execParser $ fixnixParserInfo locations
  case cmd of 
    Print x -> do
      txt <- fetchFixText x
      Text.putStr txt
    Add xs -> do
      forM_ xs $ \x -> do
        txt <- fetchFixText x
        writeDiff txt (cfgFilename cfg x)
    ListLocations -> 
      Text.putStr $ listLocations locations

 where
  writeDiff :: Text -> Path Rel File -> IO ()
  writeDiff txt file = do
    createDirectoryIfMissing True (fromRelDir $ parent file)
    let filef = fromRelFile file
    tryIOError (Text.readFile filef) >>= \case
      Left _ -> 
        hPutStrLn stderr "A new file."
      Right txt2 -> case diffText True txt2 txt of 
        Just msg -> do 
          hPutStrLn stderr ("Found a file in " <> show file)
          hPutStr stderr msg
          confirm "Can I override this file?" do
            Text.writeFile filef txt
        Nothing  -> 
          return ()

  confirm :: String -> IO () -> IO ()
  confirm help dothis = do
    hPutStr stderr (help ++ " [yes/no]: ")
    fix $ \rec -> do
      answer <- Text.strip . Text.toLower <$> Text.getLine
      case answer of
        "yes" -> dothis
        "no" -> return ()
        _ -> hPutStr stderr ("Please answer yes or no.") *> rec
    hPutStrLn stderr ""



