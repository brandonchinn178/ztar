{-# LANGUAGE QuasiQuotes #-}

import Codec.Archive.Tar.GZip (createGZ, extractGZ)
import Control.Monad (unless)
import Path
import Path.IO (ensureDir, removeDirRecur, removeFile, withSystemTempDir)

main :: IO ()
main = withSystemTempDir "" $ \dir -> do
  mapM_ (mkFile dir) files
  createGZ (fromRelFile archive) (fromAbsDir dir) ["."]

  extractGZ (fromRelDir extractDir) $ fromRelFile archive
  contents <- mapM (readFile . fromRelFile . (extractDir </>)) files
  putStrLn $ unlines contents
  unless (contents == map show files) $
    fail "Contents do not match files"

  removeFile archive
  removeDirRecur extractDir
  where
    archive = [relfile|example.tgz|]
    extractDir = [reldir|dest|]
    mkFile dir f = do
      let f' = dir </> f
      ensureDir $ parent f'
      writeFile (fromAbsFile f') (show f)
    files =
      [ [relfile|foo.txt|]
      , [relfile|bar.txt|]
      , [relfile|baz/a.txt|]
      , [relfile|baz/b.txt|]
      ]
