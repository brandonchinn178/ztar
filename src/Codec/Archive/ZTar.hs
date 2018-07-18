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
  , createFrom
  , createFrom'
  , extract
  , extract'
  ) where

import qualified Data.ByteString.Lazy as BS
import Path (Dir, File, Path, toFilePath)

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

-- | Same as 'create' but using Path types.
create' :: Compression -> Path b0 File -> Path b1 Dir -> IO ()
create' compression (toFilePath -> archive) (toFilePath -> dir) = create compression archive dir

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

-- | Same as 'createFrom' but using Path types.
createFrom' :: Compression -> Path b0 File -> Path b1 Dir -> [FilePath] -> IO ()
createFrom' compression (toFilePath -> archive) (toFilePath -> dir) paths =
  createFrom compression archive dir paths

-- | Extract an archive to the given directory. Automatically detects the compression algorithm.
-- used in the archive.
extract :: FilePath -- ^ archive to extract
        -> FilePath -- ^ destination directory
        -> IO ()
extract archive dir = BS.readFile archive >>= \case
  Tar.TarFormat -> Tar.extract archive dir
  GZip.GZipFormat -> GZip.extract archive dir
  Zip.ZipFormat -> Zip.extract archive dir
  _ -> fail $ "Could not recognize archive format: " ++ archive

-- | Same as 'extract' but using Path types.
extract' :: Path b1 File -> Path b0 Dir -> IO ()
extract' (toFilePath -> archive) (toFilePath -> dir) = extract archive dir
