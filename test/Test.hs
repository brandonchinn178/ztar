{-# LANGUAGE QuasiQuotes #-}

import Codec.Archive.ZTar
import Control.Monad (forM, forM_)
import qualified Data.ByteString as BS
import Data.ByteString.Arbitrary (ArbByteString(..))
import Data.List (isPrefixOf, nub)
import Data.Maybe (fromJust)
import Path
    ( Dir
    , File
    , Path
    , Rel
    , absdir
    , fromAbsFile
    , fromRelDir
    , parent
    , parseRelDir
    , parseRelFile
    , (</>)
    )
import Path.IO (doesFileExist, ensureDir, isLocationOccupied, withTempDir)
import qualified System.FilePath.Windows as Windows
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

main :: IO ()
main = defaultMain $ testGroup "ztar"
  [ testProperty "Create/extract uncompressed tar archives" $ testZTar 25 NoCompression
  , testProperty "Create/extract GZip tar archives" $ testZTar 20 GZip
  , testProperty "Create/extract Zip archives" $ testZTar 15 Zip
  ]

testZTar :: Int -> Compression -> Property
testZTar n compression = withMaxSuccess n $ monadicIO $ do
  [archive, src, dest] <- pick $ uniqueListOf 3 arbitrary
  files <- pick arbitraryFileTree

  run $ withTempDir [absdir|/tmp|] "" $ \dir -> do
    let archive' = dir </> toRelFile archive
        src' = dir </> toRelDir src
        dest' = dir </> toRelDir dest

    ensureDir $ parent archive'
    ensureDir src'
    ensureDir dest'

    -- write files to be bundled
    forM_ files $ \(path, contents) -> do
      let path' = src' </> path
      ensureDir $ parent path'
      BS.writeFile (fromAbsFile path') contents

    -- create and extract archive
    create' compression archive' src'
    extract' archive' dest'

    -- check files
    fmap and $ forM files $ \(path, contents) -> do
      let path' = dest' </> path
      isExist <- isLocationOccupied path'
      isFile <- doesFileExist path'
      case (isExist, isFile) of
        (False, _) -> return False
        (True, True) -> do
          contents' <- BS.readFile $ fromAbsFile path'
          return $ contents' == contents
        (True, False) -> return True

{- Helpers -}

toRelFile :: ValidName -> Path Rel File
toRelFile = fromJust . parseRelFile . unName

toRelDir :: ValidName -> Path Rel Dir
toRelDir = fromJust . parseRelDir . unName

maybeSlash :: Maybe (Path Rel Dir) -> Path Rel t -> Path Rel t
maybeSlash Nothing path = path
maybeSlash (Just dir) path = dir </> path

-- | Generate a unique list with length of N.
uniqueListOf :: Eq a => Int -> Gen a -> Gen [a]
uniqueListOf 0 _ = return []
uniqueListOf n gen = do
  rest <- uniqueListOf (n - 1) gen
  x <- gen `suchThat` (`notElem` rest)
  return $ x : rest

-- | An arbitrary file tree and their contents.
arbitraryFileTree :: Gen [(Path Rel File, BS.ByteString)]
arbitraryFileTree = mkFileTree Nothing (0 :: Int)
  where
    mkFileTree (Just dir) _
      | length (fromRelDir dir) > 50 = return []
    mkFileTree _ 5 = return [] -- max depth of 5 directories
    mkFileTree dir depth = do
      paths <- nub <$> listOf arbitrary
      fmap concat $ forM paths $ \path -> do
        -- exponentially less likely to go into subdirectories
        isFile <- frequency [(1, pure False), (2^depth, pure True)]
        if isFile
          then do
            Blind (ABS contents) <- arbitrary
            return [(dir `maybeSlash` toRelFile path, contents)]
          else mkFileTree (Just $ dir `maybeSlash` toRelDir path) (depth + 1)

-- | A valid file or directory name.
newtype ValidName = ValidName { unName :: String }
  deriving (Show,Eq)

{-# ANN module "HLint: ignore Use ||" #-}
instance Arbitrary ValidName where
  arbitrary = do
    -- https://stackoverflow.com/a/2306003/8565175
    name <- take 14 <$> listOf1 (elements validChars)
    if isValid name
      then return $ ValidName name
      else arbitrary
    where
      validChars = ['a'..'z'] ++ ['0'..'9'] ++ ['.', '_', '-']
      isValid name = not $ or
        [ last name == '.'
        -- https://superuser.com/questions/259703/get-mac-tar-to-stop-putting-filenames-in-tar-archives
        , "._" `isPrefixOf` name
        , not $ Windows.isValid name
        ]
