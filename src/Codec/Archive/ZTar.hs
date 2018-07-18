{-|
Module      :  Codec.Archive.ZTar
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Functions to create/extract archives.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Codec.Archive.ZTar
  ( Compression(..)
  , create
  , createFrom
  , extract
  ) where

import qualified Data.ByteString.Lazy as BS

import qualified Codec.Archive.ZTar.GZip as GZip
import qualified Codec.Archive.ZTar.Tar as Tar
import qualified Codec.Archive.ZTar.Zip as Zip

-- | The compression algorithm to use when creating an archive.
data Compression
  = NoCompression
  | GZip
  | Zip
  deriving (Show)

-- | Create a new archive from the given directory using the given compression algorithm.
create :: Compression
       -> FilePath -- ^ archive file to create
       -> FilePath -- ^ directory to archive
       -> IO ()
create compression archive dir = createFrom compression archive dir ["."]

-- | Create a new archive from the given paths using the given compression algorithm.
createFrom :: Compression
           -> FilePath -- ^ archive file to create
           -> FilePath -- ^ base directory
           -> [FilePath] -- ^ files and paths to compress, relative to base directory
           -> IO ()
createFrom compression = case compression of
  NoCompression -> Tar.create
  GZip -> GZip.create
  Zip -> Zip.create

-- | Extract an archive to the given directory. Automatically detects the compression algorithm.
-- used in the archive.
extract :: FilePath -- ^ destination directory
        -> FilePath -- ^ archive to extract
        -> IO ()
extract dir archive = BS.readFile archive >>= \case
  Tar.TarFormat -> Tar.extract dir archive
  GZip.GZipFormat -> GZip.extract dir archive
  Zip.ZipFormat -> Zip.extract dir archive
  _ -> fail $ "Could not recognize archive format: " ++ archive
