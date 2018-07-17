{-|
Module      :  Codec.Archive.ZTar.Path
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Types for representing filepaths.
-}
{-# LANGUAGE CPP #-}

module Codec.Archive.ZTar.Path
  ( PathFile
  , PathDir
  , toFP
  ) where

#ifdef TYPED_PATHS

import Path

type PathFile b = Path b File
type PathDir b = Path b Dir

-- | Convert a Path into a FilePath.
toFP :: Path b t -> FilePath
toFP = toFilePath

#else

type PathFile b = FilePath
type PathDir b = FilePath

-- | Convert a FilePath into a FilePath.
toFP :: FilePath -> FilePath
toFP = id

#endif
