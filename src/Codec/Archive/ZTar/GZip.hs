{-|
Module      :  Codec.Archive.ZTar.GZip
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Functions to create/extract GZIP-compressed archives.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Codec.Archive.ZTar.GZip
  ( pattern GZipFormat
  , create
  , extract
  ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZ
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

-- | A pattern matching any ByteString in the GZip format.
pattern GZipFormat :: ByteString
pattern GZipFormat <- ((BS.pack [0x1F, 0x8B] `BS.isPrefixOf`) -> True)

-- | Create a new archive compressed with GZip from the given paths.
--
-- It is equivalent to calling the standard 'tar' program like so:
--
-- @$ tar -czf archive -C base paths@
create :: FilePath -- ^ archive to create
       -> FilePath -- ^ base directory
       -> [FilePath] -- ^ files and paths to compress, relative to base directory
       -> IO ()
create archive base paths =
  BS.writeFile archive . GZ.compress . Tar.write =<< Tar.pack base paths

-- | Extract all the files contained in an archive compressed with GZip.
--
-- It is equivalent to calling the standard 'tar' program like so:
--
-- @$ tar -xf archive -C dir@
extract :: FilePath -- ^ archive to extract
        -> FilePath -- ^ destination directory
        -> IO ()
extract archive dir =
  Tar.unpack dir . Tar.read . GZ.decompress =<< BS.readFile archive
