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
  , cfgUnpack         :: !Bool
  }

data Command 
  = Print (Maybe Text, LocationFinder)
  | Add   (Maybe Text, LocationFinder)

readLocationFinder :: [LocationType] -> ReadM (Maybe Text, LocationFinder)
readLocationFinder ltps = eitherReader $ 
  parseEither parser "LOCATION" . Text.pack
 where
   parser = liftA2 (,) parseName (anyLocationFinderP ltps)
   parseName :: P (Maybe Text)
   parseName = P.optional . P.try $ P.takeWhile1P Nothing (/= '=') <* P.char '='

printCommand :: [LocationType] -> Mod CommandFields Command
printCommand ltps = command "print" $
  info p (progDesc "print the fix-file to stdout")
 where
  p = do
    x <- argument (readLocationFinder ltps) $ 
      metavar "LOCATION"
      <> help "The location to download"
    pure $ Print x


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
      , cfgUnpack = True
      }

fixnixParserInfo :: [LocationType] -> ParserInfo (Config, Command)
fixnixParserInfo ltps = info 
    (((,) <$> parseConfig ltps <*> 
      hsubparser (printCommand ltps)
    ) <**> 
    helper) $
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

fetchFixText :: (Maybe Text, LocationFinder) -> IO Text
fetchFixText (name, finder) = do
  loc <- findLocation finder
  let basename = fromMaybe (finderBaseName finder) name
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
    Print x -> 
      Text.putStr =<< fetchFixText x 

-- where
--  writeDiff :: Text -> Path Rel File -> IO ()
--  writeDiff txt file = do
--    createDirectoryIfMissing True (fromRelDir $ parent file)
--    let filef = fromRelFile file
--    txt2 <- Text.readFile filef
--    case diffText True txt2 txt of 
--      Just msg -> hPutStr stderr msg
--      Nothing  -> return ()




