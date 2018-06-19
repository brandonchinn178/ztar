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
  , pattern TarFormat
  , create
  , create'
  , extract
  ) where

import qualified Codec.Archive.Tar as Tar
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

import qualified Codec.Archive.ZTar.GZip as GZip
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
create compression archive dir = create' compression archive dir ["."]

-- | Create a new archive from the given paths using the given compression algorithm.
create' :: Compression
        -> FilePath -- ^ archive file to create
        -> FilePath -- ^ base directory
        -> [FilePath] -- ^ files and paths to compress, relative to base directory
        -> IO ()
create' compression = case compression of
  NoCompression -> Tar.create
  GZip -> GZip.create
  Zip -> Zip.create

-- | Extract an archive to the given directory. Automatically detects the compression algorithm.
-- used in the archive.
extract :: FilePath -- ^ destination directory
        -> FilePath -- ^ archive to extract
        -> IO ()
extract dir archive = BS.readFile archive >>= \case
  Zip.ZipFormat -> Zip.extract dir archive
  GZip.GZipFormat -> GZip.extract dir archive
  TarFormat -> Tar.extract dir archive
  _ -> fail $ "Could not recognize archive format: " ++ archive

-- | A pattern matching any ByteString in an uncompressed TAR format.
pattern TarFormat :: ByteString
pattern TarFormat <- (matchesTar -> True)

matchesTar :: ByteString -> Bool
matchesTar (BS.drop 0x101 -> s) = any (`BS.isPrefixOf` s) tarMagicNumbers
  where
    tarMagicNumbers = map BS.pack
      [ [0x75, 0x73, 0x74, 0x61, 0x72, 0x00, 0x30, 0x30]
      , [0x75, 0x73, 0x74, 0x61, 0x72, 0x20, 0x20, 0x00]
      ]
