{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Exception.Safe (throwString)
import Data.ByteString qualified as B
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (for_)
import Data.Scientific (Scientific)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TEL
import Data.Text.Lazy.IO qualified as TLIO
import GraphQLParser
import Prettyprinter (pretty)
import System.Directory (listDirectory)
import System.FilePath
import Test.Hspec
import Test.Hspec.Golden
import Test.QuickCheck qualified as Q
import Test.QuickCheck.Arbitrary.Generic qualified as QAG
import Text.Pretty.Simple (pShowNoColor)
import Text.Read (readEither)

--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  parserSpec
  prettySpec

--------------------------------------------------------------------------------
-- Parsing tests.

type ParserOf a = B.ByteString -> Either ParseError a

-- | Parser tests.
parserSpec :: Spec
parserSpec = describe "Parser" $ do
  parserGoldenSpec runParseGraphQL "test/data/parser/executableDocument"
  parserGoldenSpec runParseGraphQL "test/data/parser/typeSystemDocument"

-- | 'Golden' parser tests for each of the files in the @examples@ subdirectory
-- found in the project directory hard-coded into this function.
parserGoldenSpec :: ParserOf GraphQLDocument -> FilePath -> Spec
parserGoldenSpec runParse testPath = describe "Golden" $ do
  (dirSuc, examplePaths) <- runIO $ fetchGoldenFiles testPath

  describe "Success" $
    for_ examplePaths $ \path -> do
      let name = takeBaseName path
      before (parseGraphQLSuccess runParse path) $
        after (writePrettyExample path . snd) $
          it ("parses " <> name) $
            \(_, valueExt) -> goldenQuery dirSuc name valueExt

-- | Parse a template file that is expected to succeed; parse failures are
-- rendered as 'String's and thrown in 'IO'.
parseGraphQLSuccess :: ParserOf GraphQLDocument -> FilePath -> IO (BS.ByteString, GraphQLDocument)
parseGraphQLSuccess runParse path = do
  tmpl <- BS.readFile path
  case runParse tmpl of
    Left err -> throwString $ "Unexpected parsing failure " <> show err
    Right gql -> pure (tmpl, gql)

--------------------------------------------------------------------------------
-- Pretty Printer Round Trip.

-- | Parser tests.
prettySpec :: Spec
prettySpec = describe "Pretty Printer" $ do
  prettyGoldenSpec "test/data/printer"

-- | 'Golden' parser tests for each of the files in the @examples@ subdirectory
-- found in the project directory hard-coded into this function.
prettyGoldenSpec :: FilePath -> Spec
prettyGoldenSpec testPath = describe "Golden" $ do
  (dirSuc, examplePaths) <- runIO $ fetchGoldenFiles $ testPath

  describe "Success" $
    for_ examplePaths $ \path -> do
      let name = takeFileName path
      before (readPrettyExample path) $
        it ("pretty prints " <> name) $
          \valueExt -> do
            let prettyTerm = TE.encodeUtf8 $ renderPretty valueExt
            runParseGraphQL prettyTerm `shouldBe` Right valueExt

-- | Write serialized haskell terms to printer example folder for
-- pretty printer round trip.
writePrettyExample :: FilePath -> GraphQLDocument -> IO ()
writePrettyExample path =
  let newPath = "test/data/printer/examples" </> takeBaseName path
   in BS.writeFile newPath . BL.toStrict . TEL.encodeUtf8 . pShowNoColor

readPrettyExample :: FilePath -> IO GraphQLDocument
readPrettyExample path = do
  eVal <- readEither . TL.unpack <$> TLIO.readFile path
  either throwString pure eVal

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
goldenQuery :: FilePath -> String -> GraphQLDocument -> Golden GraphQLDocument
goldenQuery = goldenReadShow

-- | Construct a 'Golden' test for 'ParseError's rendered as 'String's.
--
-- Since 'ParseError' doesn't export a 'Show' instance that satisfies the
-- 'Read' <-> 'Show' roundtrip law, we must deal with its errors in terms of
-- the text it produces.
-- goldenParseError :: FilePath -> String -> ParseError -> Golden String
-- goldenParseError dir name parseError' = Golden {..}
--  where
--    output = show parseError'
--    encodePretty = id
--    writeToFile path actual = BS.writeFile path . TE.encodeUtf8 . T.pack $ actual
--    readFromFile path = T.unpack . TE.decodeUtf8 <$> BS.readFile path
--    goldenFile = dir </> "golden" </> name <.> "txt"
--    actualFile = Just $ dir </> "actual" </> name <.> "txt"
--    failFirstTime = False

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
