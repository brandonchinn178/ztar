{-# LANGUAGE CPP #-}

module Utilities where

import Path

#ifdef TYPED_PATHS
fromPath :: Path b t -> Path b t
fromPath = id
#else
fromPath :: Path b t -> FilePath
fromPath = toFilePath
#endif
