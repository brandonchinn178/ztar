{-|
Module      :  Codec.Archive.ZTar.Tar
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Functions to create/extract uncompressed tar archives.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Codec.Archive.ZTar.Tar
  ( pattern TarFormat
  , create
  , extract
  ) where

import qualified Codec.Archive.Tar as Tar
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

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

-- | Create a new uncompressed tar archive from the given paths.
--
-- It is equivalent to calling the standard 'tar' program like so:
--
-- @$ tar -cf archive -C base paths@
create :: FilePath -- ^ archive to create
       -> FilePath -- ^ base directory
       -> [FilePath] -- ^ files and paths to compress, relative to base directory
       -> IO ()
create = Tar.create

-- | Extract all the files contained in an uncompressed tar archive.
--
-- It is equivalent to calling the standard 'tar' program like so:
--
-- @$ tar -xf archive -C dir@
extract :: FilePath -- ^ archive to extract
        -> FilePath -- ^ destination directory
        -> IO ()
extract = flip Tar.extract
