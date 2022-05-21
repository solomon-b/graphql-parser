{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Exception.Safe (throwString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (for_)
#if !MIN_VERSION_aeson(2,0,3)
import qualified Data.Vector as V
#endif
import Data.Scientific (Scientific)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TEL
import qualified Data.Text.Lazy.IO as TLIO
import GraphQLParser
import System.Directory (listDirectory)
import System.FilePath
import Test.Hspec
import Test.Hspec.Golden
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Arbitrary.Generic as QAG
import Text.Pretty.Simple (pShowNoColor)
import Text.Read (readEither)

--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  parserSpec

--------------------------------------------------------------------------------
-- Parsing tests.

-- | Parser tests.
parserSpec :: Spec
parserSpec = describe "Parser" $ do
  parserGoldenSpec

-- | 'Golden' parser tests for each of the files in the @examples@ subdirectory
-- found in the project directory hard-coded into this function.
parserGoldenSpec :: Spec
parserGoldenSpec = describe "Golden" $ do
  (dirSuc, pathsSuc) <- runIO $ fetchGoldenFiles "test/data/success"
  (dirFail, pathsFail) <- runIO $ fetchGoldenFiles "test/data/failure"

  describe "Success" $
    for_ pathsSuc $ \path -> do
      let name = dropExtension $ takeFileName path
      before (parseGraphQLSuccess path) $
        it ("parses " <> name) $
          \(_, valueExt) -> goldenQuery dirSuc name valueExt

  describe "Failure" $
    for_ pathsFail $ \path -> do
      let name = dropExtension $ takeFileName path
      before (parseGraphQLFailure path) $
        it ("fails to parse " <> name) $
          \parseError' -> goldenParseError dirFail name parseError'

-- | Parse a template file that is expected to succeed; parse failures are
-- rendered as 'String's and thrown in 'IO'.
parseGraphQLSuccess :: FilePath -> IO (BS.ByteString, ExecutableDocument)
parseGraphQLSuccess path = do
  tmpl <- BS.readFile $ path
  case runParseExecutable tmpl of
    Left err -> throwString $ "Unexpected parsing failure " <> show err
    Right gql -> pure (tmpl, gql)

-- | Parse a template file that is expected to fail.
parseGraphQLFailure :: FilePath -> IO ParseError
parseGraphQLFailure path = do
  tmpl <- BS.readFile $ path
  case runParseExecutable tmpl of
    Left err -> pure err
    Right gql -> throwString $ "Unexpected parsing success " <> show gql

--------------------------------------------------------------------------------
-- Golden test construction functions.

-- | Construct a 'Golden' test for any value with valid 'Read' and 'Show'
-- instances.
--
-- In this case, "valid" means that the value satisfies the roundtrip law where
-- @read . show === id@.
goldenReadShow ::
  (Read val, Show val) => FilePath -> String -> val -> Golden val
goldenReadShow dir name val = Golden {..}
  where
    output = val
    encodePretty = TL.unpack . pShowNoColor
    writeToFile path actual =
      BS.writeFile path . BL.toStrict . TEL.encodeUtf8 . pShowNoColor $ actual
    readFromFile path = do
      eVal <- readEither . TL.unpack <$> TLIO.readFile path
      either throwString pure eVal
    goldenFile = dir </> "golden" </> name <.> "txt"
    actualFile = Just $ dir </> "actual" </> name <.> "txt"
    failFirstTime = False

-- | Alias for 'goldenReadShow' specialized to 'ExecutableDocument'.
goldenQuery :: FilePath -> String -> ExecutableDocument -> Golden ExecutableDocument
goldenQuery = goldenReadShow

-- | Construct a 'Golden' test for 'ParseError's rendered as 'String's.
--
-- Since 'ParseError' doesn't export a 'Show' instance that satisfies the
-- 'Read' <-> 'Show' roundtrip law, we must deal with its errors in terms of
-- the text it produces.
goldenParseError :: FilePath -> String -> ParseError -> Golden String
goldenParseError dir name parseError' = Golden {..}
  where
    output = show parseError'
    encodePretty = id
    writeToFile path actual = BS.writeFile path . TE.encodeUtf8 . T.pack $ actual
    readFromFile path = T.unpack . TE.decodeUtf8 <$> BS.readFile path
    goldenFile = dir </> "golden" </> name <.> "txt"
    actualFile = Just $ dir </> "actual" </> name <.> "txt"
    failFirstTime = False

--------------------------------------------------------------------------------
-- QuickCheck helpers and orphan instances.

alphabet :: String
alphabet = ['a' .. 'z'] ++ ['A' .. 'Z']

alphaNumerics :: String
alphaNumerics = alphabet ++ "0123456789"

instance Q.Arbitrary T.Text where
  arbitrary = do
    x <- Q.listOf1 (Q.elements alphabet)
    y <- Q.listOf1 (Q.elements alphaNumerics)
    pure $ T.pack $ x <> y

instance Q.Arbitrary Scientific where
  arbitrary = ((fromRational . toRational) :: Int -> Scientific) <$> Q.arbitrary

instance Q.Arbitrary AlexSourcePos where
  arbitrary = QAG.genericArbitrary
  shrink = QAG.genericShrink

instance Q.Arbitrary Symbol where
  arbitrary = QAG.genericArbitrary

instance Q.Arbitrary Token where
  arbitrary =
    QAG.genericArbitrary >>= \case
      TokNumLit _ i -> pure $ TokNumLit (T.pack $ show $ unLoc i) i
      TokIntLit i -> pure $ TokIntLit i
      EOF -> Q.arbitrary
      val -> pure val

instance (Q.Arbitrary a) => Q.Arbitrary (Loc a) where
  arbitrary = QAG.genericArbitrary
  shrink = QAG.genericShrink

instance Q.Arbitrary Span where
  arbitrary = QAG.genericArbitrary
  shrink = QAG.genericShrink

#if !MIN_VERSION_aeson(2,0,3)
instance Q.Arbitrary J.Value where
  arbitrary = Q.sized sizedArbitraryValue
    where
      sizedArbitraryValue n
        | n <= 0 = Q.oneof [pure J.Null, boolean', number', string']
        | otherwise = Q.resize n' $ Q.oneof [pure J.Null, boolean', number', string', array', object']
        where
          n' = n `div` 2
          boolean' = J.Bool <$> Q.arbitrary
          number' = J.Number <$> Q.arbitrary
          string' = J.String <$> Q.arbitrary
          array' = J.Array . V.fromList <$> Q.arbitrary
          object' = J.Object . Compat.fromList <$> Q.arbitrary
#endif

--------------------------------------------------------------------------------
-- General test helpers.

-- | Fetches example files for golden tests from the @examples@ subdirectory
-- at the given 'FilePath'.
--
-- We assume that the directory at the given 'FilePath' has the following
-- structure:
--  * /actual
--  * /examples
--  * /golden
fetchGoldenFiles :: FilePath -> IO (FilePath, [FilePath])
fetchGoldenFiles dir = do
  let exampleDir = dir </> "examples"
  examples <- listDirectory exampleDir
  pure (dir, map (exampleDir </>) examples)
