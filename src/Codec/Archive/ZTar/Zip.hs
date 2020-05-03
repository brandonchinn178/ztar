{-|
Module      :  Codec.Archive.ZTar.Zip
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Functions to create/extract ZIP-compressed archives.
-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Codec.Archive.ZTar.Zip
  ( pattern ZipFormat
  , create
  , extract
  ) where

import qualified Codec.Archive.Zip as Zip
import Control.Monad.Extra (concatMapM)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , listDirectory
    , makeAbsolute
    , withCurrentDirectory
    )
import System.FilePath ((</>))

-- | A pattern matching any ByteString in the Zip format.
pattern ZipFormat :: ByteString
pattern ZipFormat <- (matchesZip -> True)

matchesZip :: ByteString -> Bool
matchesZip s = any (`BS.isPrefixOf` s) zipMagicNumbers
  where
    zipMagicNumbers = map BS.pack
     [ [0x50, 0x4B, 0x03, 0x04]
     -- empty archive
     , [0x50, 0x4B, 0x05, 0x06]
     -- spanned archive
     , [0x50, 0x4B, 0x07, 0x08]
     ]

-- | Create a new archive compressed with Zip from the given paths.
--
-- It is equivalent to calling the standard 'zip' program like so:
--
-- @$ CURR=$PWD; (cd base && zip -r archive paths && mv archive $CURR)@
create :: FilePath -- ^ archive to create
       -> FilePath -- ^ base directory
       -> [FilePath] -- ^ files and paths to compress, relative to base directory
       -> IO ()
create archive base paths = do
  archive' <- makeAbsolute archive
  withCurrentDirectory base $ do
    files <- concatMapM search paths
    Zip.createArchive archive' $ mapM_ insertFile files
  where
    search :: FilePath -> IO [FilePath]
    search path = do
      isFile <- doesFileExist path
      isDir <- doesDirectoryExist path
      if
        | isFile -> pure [path]
        | isDir -> searchDir path
        | otherwise -> fail $ "Path does not exist: " ++ path
    searchDir :: FilePath -> IO [FilePath]
    searchDir path =
      let mkPath = if path == "." then id else (path </>)
      in concatMapM (search . mkPath) =<< liftIO (listDirectory path)
    insertFile path = do
      path' <- Zip.mkEntrySelector path
      Zip.loadEntry Zip.BZip2 path' path

-- | Extract all the files contained in an archive compressed with Zip.
--
-- It is equivalent to calling the standard 'zip' program like so:
--
-- @$ mkdir -p dir && unzip archive -d dir@
extract :: FilePath -- ^ archive to extract
        -> FilePath -- ^ destination directory
        -> IO ()
extract archive dir = do
  createDirectoryIfMissing True dir
  Zip.withArchive archive $ Zip.unpackInto dir
