{-|
Module      :  Codec.Archive.ZTar.Path
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Types for representing filepaths.

The types in this file are different depending on whether the @typed-paths@ flag was enabled.
-}
{-# LANGUAGE CPP #-}

module Codec.Archive.ZTar.Path
  ( PathFile
  , PathDir
  , toFP
  ) where

#ifdef TYPED_PATHS

import Path

-- | The type for a path to a file.
--
-- With untyped paths, this becomes @FilePath@.
type PathFile b = Path b File

-- | The type for a path to a directory.
--
-- With untyped paths, this becomes @FilePath@.
type PathDir b = Path b Dir

-- | Convert a path into a FilePath.
--
-- With untyped paths, this becomes @FilePath -> FilePath@
toFP :: Path b t -> FilePath
toFP = toFilePath

#else

-- | The type for a path to a file.
--
-- With typed paths, this becomes @Path b File@.
type PathFile b = FilePath

-- | The type for a path to a directory.
--
-- With typed paths, this becomes @Path b Dir@.
type PathDir b = FilePath

-- | Convert a path into a FilePath.
--
-- With typed paths, this becomes @Path b t -> FilePath@
toFP :: FilePath -> FilePath
toFP = id

#endif
