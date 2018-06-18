{-|
Module      :  Codec.Archive.Tar.GZip
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Functions to create/extract compressed tar archives.
-}

module Codec.Archive.Tar.GZip (createGZ, createGZ', extractGZ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZ
import qualified Data.ByteString.Lazy as BS

-- | Create a new @.tar@ file from the given paths.
--
-- It is equivalent to calling the standard 'tar' program like so:
--
-- @$ tar -czf tarball.tar -C base [paths]@
--
-- See 'Tar.create' for more details.
createGZ :: FilePath -> FilePath -> [FilePath] -> IO ()
createGZ tar base paths = BS.writeFile tar . GZ.compress . Tar.write =<< Tar.pack base paths

-- | Create a new @.tar@ file from the given directory.
--
-- It is equivalent to calling the standard 'tar' program like so:
--
-- @$ tar -czf tarball.tar -C dir .@
--
-- See 'Tar.create' for more details.
createGZ' :: FilePath -> FilePath -> IO ()
createGZ' tar dir = createGZ tar dir ["."]

-- | Extract all the files contained in a @.tar@ file.
--
-- It is equivalent to calling the standard 'tar' program like so:
--
-- @$ tar -xzf tarball.tar -C dir@
--
-- See 'Tar.extract' for more details.
extractGZ :: FilePath -> FilePath -> IO ()
extractGZ dir tar = Tar.unpack dir . Tar.read . GZ.decompress =<< BS.readFile tar
