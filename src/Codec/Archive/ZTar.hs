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
  , create'
  , extract
  ) where

import qualified Data.ByteString.Lazy as BS

import qualified Codec.Archive.ZTar.GZip as GZip
import Codec.Archive.ZTar.Path
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
       -> PathFile b0 -- ^ archive file to create
       -> PathDir b1 -- ^ directory to archive
       -> IO ()
create compression archive dir = create' compression archive dir ["."]

-- | Create a new archive from the given paths using the given compression algorithm.
create' :: Compression
        -> PathFile b0 -- ^ archive file to create
        -> PathDir b1 -- ^ base directory
        -> [FilePath] -- ^ files and paths to compress, relative to base directory
        -> IO ()
create' compression = case compression of
  NoCompression -> Tar.create
  GZip -> GZip.create
  Zip -> Zip.create

-- | Extract an archive to the given directory. Automatically detects the compression algorithm.
-- used in the archive.
extract :: PathDir b0 -- ^ destination directory
        -> PathFile b1 -- ^ archive to extract
        -> IO ()
extract dir archive = BS.readFile (toFP archive) >>= \case
  Tar.TarFormat -> Tar.extract dir archive
  GZip.GZipFormat -> GZip.extract dir archive
  Zip.ZipFormat -> Zip.extract dir archive
  _ -> fail $ "Could not recognize archive format: " ++ (toFP archive)
