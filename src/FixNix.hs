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

data LocationConfig = LocationConfig
  { cfgUserDefinedBaseName :: Maybe Text
  , cfgLocationFinder :: LocationFinder
  , cfgUnpack :: Maybe Bool
  }

cfgBaseName :: LocationConfig -> Text
cfgBaseName LocationConfig {..} = 
  fromMaybe (finderBaseName cfgLocationFinder) cfgUserDefinedBaseName

cfgFilename :: Config -> LocationConfig -> Path Rel File
cfgFilename Config {..} lc@LocationConfig {..} = 
  case parseRelFile filename of
   Just rf -> cfgFixFolder </> rf
   Nothing -> error ("Bad base name " ++ filename)
 where 
  filename = Text.unpack $ cfgBaseName lc <> ".nix"

data Command 
  = Print LocationConfig
  | Add   [LocationConfig]
  | ListLocations

readLocationFinder :: [LocationType] -> ReadM LocationConfig
readLocationFinder ltps = eitherReader $ 
  parseEither parser "LOCATION" . Text.pack
 where
   parser = do
     cfgUserDefinedBaseName <- parseName
     cfgUnpack <- optional (msum [ P.string "!" $> True, P.string "!!" $> False ])
     cfgLocationFinder <- anyLocationFinderP ltps
     pure $ LocationConfig {..}
   parseName :: P (Maybe Text)
   parseName = P.optional . P.try $ P.takeWhile1P Nothing (/= '=') <* P.char '='

argLocation :: [LocationType] -> Parser LocationConfig
argLocation ltps = argument (readLocationFinder ltps) $ 
  metavar "LOCATION"
  <> help "The location to download (see 'fixnix list)"

printCommand :: [LocationType] -> Mod CommandFields Command
printCommand ltps = command "print" $
  info p (progDesc "print the fix-file to stdout")
 where
  p = do
    x <- argLocation ltps
    pure $ Print x

addCommand :: [LocationType] -> Mod CommandFields Command
addCommand ltps = command "add" $
  info p (progDesc "add/override fix-file(s) to the fix-folder")
 where
  p = do
    x <- many $ argLocation ltps
    pure $ Add x

listCommand :: [LocationType] -> Mod CommandFields Command
listCommand ltps = command "list" $
  info (pure $ ListLocations) (progDesc "list the locations and their formats")


parseConfig :: [LocationType] -> Parser Config
parseConfig ltps = do
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
    (((,) <$> parseConfig ltps <*> 
      hsubparser (printCommand ltps <> addCommand ltps <> listCommand ltps)
    ) <**> 
    helper) $
  fullDesc
  <> header ("fixnix - a nix expression fixer (version " ++ showVersion Paths_fixnix.version ++ ")")
  <> footerDoc (Just mempty)


listLocations :: [LocationType] -> IO ()
listLocations ltps = D.putDoc $ D.vsep
  [ "Below is a list of defined locations." D.</> "If the list is incomplete" D.</> "please" 
    D.</> "file a bug report to https://github.com/kalhauge/fixnix."
  , ""
  , D.vcat $ map (D.nest 2 . describeLocationType) ltps
  ]

fetchFixText :: Text -> LocationFinder -> IO Text
fetchFixText basename finder = do
  loc <- findLocation finder
  sha256 <- prefetchIO basename loc
  case sha256 of 
    Nothing -> fail "Could not prefetch url."
    Just sha256 -> do 
      args <- getArgs
      return $ renderFixNixExpr args basename loc sha256
 where 
  renderFixNixExpr args basename loc sha256 = foldMap (<> "\n")
    [ "# Auto-generated with fixnix (version " <> textVersion <> ")"
    , "# " <> Text.unwords (map Text.pack args)
    , renderLocation basename loc sha256
    ]
   where
    textVersion = Text.pack $ showVersion Paths_fixnix.version

-- | Run the io
run :: IO ()
run = do
  (cfg@Config {..}, cmd) <- execParser $ fixnixParserInfo locations
  case cmd of 
    Print x -> do
      txt <- fetchFixText (cfgBaseName x) (cfgLocationFinder x)
      Text.putStr txt
    Add xs -> do
      forM_ xs $ \x -> do
        txt <- fetchFixText (cfgBaseName x) (cfgLocationFinder x)
        writeDiff txt (cfgFilename cfg x)
    ListLocations -> 
      listLocations locations

 where
  writeDiff :: Text -> Path Rel File -> IO ()
  writeDiff txt file = do
    createDirectoryIfMissing True (fromRelDir $ parent file)
    let filef = fromRelFile file
    txt2 <- Text.readFile filef
    case diffText True txt2 txt of 
      Just msg -> do 
        hPutStr stderr ("Found a file in " <> show file)
        hPutStr stderr msg
        confirm "Can I override this file?" do
          Text.writeFile filef txt

      Nothing  -> 
        return ()

  confirm :: String -> IO () -> IO ()
  confirm help dothis = do
    hPutStr stderr (help ++ " [yes/no]")
    fix $ \rec -> do
      answer <- Text.strip . Text.toLower <$> Text.getLine
      case answer of
        "yes" -> dothis
        "no" -> return ()
        _ -> hPutStr stderr ("Please answer yes or no.") *> rec



