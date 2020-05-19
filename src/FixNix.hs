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

-- path
import           Path

-- fixnix
import           Paths_fixnix
import           FixNix.Core
import           FixNix.Locations

data Config = Config
  { cfgFixFile      :: !(Maybe (Path Rel File))
  , cfgLocationFinder :: !LocationFinder
  , cfgName           :: !(Maybe Text)
  , cfgUnpack         :: !Bool
  }

data Commands 
  = Print

readLocationFinder :: [LocationType] -> ReadM LocationFinder
readLocationFinder ltps = eitherReader $ 
  parseEither (anyLocationFinderP ltps) "LOCATION" . Text.pack

parseConfig :: [LocationType] -> Parser Config
parseConfig ltps = do
  cfgLocationFinder <- argument (readLocationFinder ltps) $ 
    metavar "LOCATION"
    <> help "The location to download"

  cfgName <- optional . strOption $
    long "name"
    <> metavar "NAME"
    <> help "The name of the fix deriviation."
  
  cfgFixFile <- optional . option (maybeReader parseRelFile) $
    short 'o'
    <> long "output"
    <> hidden
    <> metavar "FILE"
    <> help "The relative path to the fix file."
  
  cfgUnpack <- switch $
    long "unpack"
    <> short 'u'
    <> help "Should the item be unpacked? (tar only)"
    <> hidden

  pure $ Config 
    { cfgFixFile
    , cfgLocationFinder
    , cfgName
    , cfgUnpack
    }

fixnixParserInfo :: [LocationType] -> ParserInfo Config 
fixnixParserInfo ltps = info (parseConfig ltps <**> helper) $
  fullDesc
  <> header ("fixnix - a nix version fixer (version " ++ showVersion Paths_fixnix.version ++ ")")
  <> footerDoc (Just footer)
 where
  footer = D.vcat
    [ "Below is a list of defined locations." D.</> "If the list is incomplete" D.</> "please" 
      D.</> "file a bug report to https://github.com/kalhauge/fixnix."
    , ""
    , D.vcat $ map (D.nest 2 . describeLocationType) ltps
    ]

-- | Run the io
run :: IO ()
run = do
  Config {..} <- execParser $ fixnixParserInfo locations
  
  loc <- findLocation cfgLocationFinder

  sha256 <- prefetchIO loc
  print sha256
  txt <- case sha256 of 
    Nothing -> fail "Could not prefetch url."
    Just sha256 -> do 
      args <- getArgs
      return $ renderFixNixExpr args loc sha256
  case cfgFixFile of
    Just fixfile -> do
      writeDiff txt fixfile
    Nothing -> 
      Text.putStr txt

 where 
  renderFixNixExpr args loc sha256 = foldMap (<> "\n")
    [ "# Auto-generated with fixnix (version " <> textVersion <> ")"
    , "# fixnix " <> Text.unwords (map Text.pack args)
    , renderLocation loc sha256
    ]
   where
    textVersion = Text.pack $ showVersion Paths_fixnix.version

  writeDiff :: Text -> Path Rel File -> IO ()
  writeDiff txt file = do
    createDirectoryIfMissing True (fromRelDir $ parent file)
    let filef = fromRelFile file
    txt2 <- Text.readFile filef
    case diffText True txt2 txt of 
      Just msg -> hPutStr stderr msg
      Nothing  -> return ()




