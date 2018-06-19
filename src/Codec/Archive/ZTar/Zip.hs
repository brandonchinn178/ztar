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
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , doesDirectoryExist
    , listDirectory
    , renameFile
    , withCurrentDirectory
    )
import System.FilePath ((</>))

-- | A pattern matching any ByteString in the Zip format.
pattern ZipFormat :: ByteString
pattern ZipFormat <- ((BS.pack [0x50, 0x4B, 0x03, 0x04] `BS.isPrefixOf`) -> True)

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
  withCurrentDirectory base $ Zip.createArchive archive $ mapM_ insert paths
  renameFile (base </> archive) archive
  where
    insert path = do
      isFile <- liftIO $ doesFileExist path
      isDir <- liftIO $ doesDirectoryExist path
      if
        | isFile -> insertFile path
        | isDir -> insertDir path
        | otherwise -> fail $ "Path does not exist: " ++ path
    insertFile path = do
      path' <- Zip.mkEntrySelector path
      Zip.loadEntry Zip.BZip2 path' path
    insertDir path =
      let mkPath = if path == "." then id else (path </>)
      in mapM_ (insert . mkPath) =<< liftIO (listDirectory path)

-- | Extract all the files contained in an archive compressed with Zip.
--
-- It is equivalent to calling the standard 'zip' program like so:
--
-- @$ mkdir -p dir && unzip archive -d dir@
extract :: FilePath -- ^ destination directory
        -> FilePath -- ^ archive to extract
        -> IO ()
extract dir archive = do
  createDirectoryIfMissing True dir
  Zip.withArchive archive $ Zip.unpackInto dir
