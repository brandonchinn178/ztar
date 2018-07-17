{-# LANGUAGE QuasiQuotes #-}

import Codec.Archive.ZTar
import Control.Monad (forM, forM_)
import Control.Monad.Extra (unlessM)
import Data.List (dropWhileEnd, intercalate, nub)
import Data.Maybe (fromJust)
import Path
    ( Dir
    , File
    , Rel
    , Path
    , absdir
    , fromAbsDir
    , fromAbsFile
    , parent
    , parseRelDir
    , parseRelFile
    , (</>)
    )
import Path.IO (doesFileExist, ensureDir, isLocationOccupied, withTempDir)
import Test.QuickCheck (Arbitrary(..), Gen, Property, elements, listOf, listOf1, suchThat)
import Test.QuickCheck.Monadic (monadicIO, pick, run)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

main :: IO ()
main = defaultMain $ testGroup "ztar"
  [ testProperty "Create/extract uncompressed tar archives" $ testZTar NoCompression
  , testProperty "Create/extract GZip tar archives" $ testZTar GZip
  , testProperty "Create/extract Zip archives" $ testZTar Zip
  ]

testZTar :: Compression -> Property
testZTar compression = monadicIO $ do
  archive:src:dest:paths <- pick $ uniqueListOf 4 arbitrary

  run $ withTempDir [absdir|/tmp|] "" $ \dir -> do
    let paths' = map toRelFile paths
        archive' = dir </> toRelFile archive
        src' = dir </> toRelDir src
        dest' = dir </> toRelDir dest

    -- write files to be bundled
    forM_ paths' $ \path -> do
      let path' = src' </> path
      -- case writing `a` when `a/b` already exists
      unlessM (isLocationOccupied path') $ do
        ensureDir $ parent path'
        writeFile (fromAbsFile path') (show path)

    -- create and extract archive
    ensureDir $ parent archive'
    create compression (fromAbsFile archive') (fromAbsDir src')
    extract (fromAbsDir dest') (fromAbsFile archive')

    -- check files
    fmap and $ forM paths' $ \path -> do
      let path' = dest' </> path
      isExist <- isLocationOccupied path'
      isFile <- doesFileExist path'
      case (isExist, isFile) of
        (False, _) -> return False
        (True, True) -> do
          contents <- readFile $ fromAbsFile path'
          return $ contents == show path
        (True, False) -> return True

{- Helpers -}

-- | Generate a unique list with length at least N.
uniqueListOf :: Eq a => Int -> Gen a -> Gen [a]
uniqueListOf 0 gen = nub <$> listOf gen
uniqueListOf n gen = do
  rest <- uniqueListOf (n - 1) gen
  x <- gen `suchThat` (`notElem` rest)
  return $ x : rest

-- | A valid relative file path.
newtype ValidPath = ValidPath { unPath :: FilePath }
  deriving (Show,Eq)

instance Arbitrary ValidPath where
  arbitrary = ValidPath . truncatePath . intercalate "/" . map unName <$> listOf1 arbitrary
    where
      truncatePath = dropWhileEnd (`elem` ['/', '.']) . take 50

toRelFile :: ValidPath -> Path Rel File
toRelFile = fromJust . parseRelFile . unPath

toRelDir :: ValidPath -> Path Rel Dir
toRelDir = fromJust . parseRelDir . unPath

-- | A valid file or directory name.
newtype ValidName = ValidName { unName :: String }
  deriving (Show)

instance Arbitrary ValidName where
  arbitrary = do
    -- https://stackoverflow.com/a/2306003/8565175
    name <- take 14 <$> listOf1 (elements validChars)
    if or
      [ last name == '.'
      , name `elem` ["nul"]
      -- https://superuser.com/questions/259703/get-mac-tar-to-stop-putting-filenames-in-tar-archives
      , name !! 0 == '.' && name !! 1 == '_'
      ]
      then arbitrary
      else return $ ValidName name
    where
      validChars = ['a'..'z'] ++ ['0'..'9'] ++ ['.', '_', '-']
