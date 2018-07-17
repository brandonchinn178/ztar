{-|
Module      :  Codec.Archive.Tar
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

The Haskell @tar@ library has a lot of bugs, including:

* Does not preserve executability (https://github.com/haskell/tar/issues/25)
* Does not preserve symbolic links (https://github.com/haskell/tar/issues/34)
* Does not allow symbolic links to `..` in a nested directory (https://github.com/haskell/tar/issues/32)

Because of these bugs, we will be using the @tar@ Unix command, which should be commonly available
on most systems. This module should be deprecated when the Haskell @tar@ library is updated.

Note that this module contains the same security considerations as the @tar@ Unix command. Follow
the same security guidelines you would with the @tar@ Unix command when using this module.
-}
{-# LANGUAGE LambdaCase #-}

module Codec.Archive.Tar where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Data.ByteString.Lazy as BS
import GHC.IO.Handle (hClose)
import Prelude hiding (read)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Exit (ExitCode(..), exitWith)
import System.PosixCompat.Temp (mkstemp)
import System.Process (readProcessWithExitCode)

-- | Runs the UNIX @tar@ command with the given arguments.
tar :: [String] -> IO ()
tar args = readProcessWithExitCode "tar" args "" >>= \case
  (ExitSuccess, _, _) -> return ()
  (ExitFailure n, _, _) -> exitWith $ ExitFailure n

create :: FilePath -> FilePath -> [FilePath] -> IO ()
create archive base paths = BS.writeFile archive . write =<< pack base paths

extract :: FilePath -> FilePath -> IO ()
extract dir archive = unpack dir . read =<< BS.readFile archive

newtype TarArchive = TarArchive { unTar :: BS.ByteString }

read :: BS.ByteString -> TarArchive
read = TarArchive

write :: TarArchive -> BS.ByteString
write = unTar

pack :: FilePath -> [FilePath] -> IO TarArchive
pack base paths = do
  (archive, h) <- mkstemp "tar-pack"
  hClose h
  tar $ ["-cf", archive, "-C", base] ++ paths
  contents <- evaluate . force =<< BS.readFile archive
  removeFile archive
  return $ TarArchive contents

unpack :: FilePath -> TarArchive -> IO ()
unpack dir (TarArchive contents) = do
  (archive, h) <- mkstemp "tar-unpack"
  hClose h
  BS.writeFile archive contents
  createDirectoryIfMissing True dir
  tar ["-xf", archive, "-C", dir]
  removeFile archive
