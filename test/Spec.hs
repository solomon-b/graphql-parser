import Control.Exception.Safe (throwString)
import Data.ByteString qualified as B
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (for_)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TEL
import Data.Text.Lazy.IO qualified as TLIO
import GraphQLParser
import System.Directory (listDirectory)
import System.FilePath
import Test.Hspec
import Test.Hspec.Golden
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
  parserGoldenSpec parseGraphQL "test/data/parser"

-- | 'Golden' parser tests for each of the files in the @examples@ subdirectory
-- found in the project directory hard-coded into this function.
parserGoldenSpec :: ParserOf Document -> FilePath -> Spec
parserGoldenSpec runParse testPath = describe "Golden" $ do
  (dirSuc, examplePaths) <- runIO $ fetchGoldenFiles testPath

  describe "Success" $
    for_ examplePaths $ \path -> do
      let name = takeBaseName path
      before (parseGraphQLSuccess runParse path) $
        it ("parses " <> name) $
          \(_, valueExt) -> goldenDocument dirSuc name valueExt

-- | Parse a template file that is expected to succeed; parse failures are
-- rendered as 'String's and thrown in 'IO'.
parseGraphQLSuccess :: ParserOf Document -> FilePath -> IO (BS.ByteString, Document)
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
  (dirSuc, examplePaths) <- runIO $ fetchGoldenFiles testPath

  describe "Success" $
    for_ examplePaths $ \path -> do
      let name = takeFileName path
      before (readPrettyExample path) $
        it ("pretty prints " <> name) $
          \valueExt -> do
            let prettyTerm = renderPrettyBS valueExt
            goldenByteString dirSuc name prettyTerm "gql"

readPrettyExample :: FilePath -> IO Document
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
goldenDocument :: FilePath -> String -> Document -> Golden Document
goldenDocument dir name val = Golden {..}
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

goldenByteString :: FilePath -> String -> BS.ByteString -> String -> Golden BS.ByteString
goldenByteString dir name val ext = Golden {..}
  where
    output = val
    encodePretty = TL.unpack . pShowNoColor
    writeToFile path actual =
      BS.writeFile path actual
    readFromFile path = BS.readFile path
    goldenFile = dir </> "golden" </> name <.> ext
    actualFile = Just $ dir </> "actual" </> name <.> ext
    failFirstTime = False

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

--batchProcess :: IO ()
--batchProcess = do
--  (_dirSuc, examplePaths) <- fetchGoldenFiles "test/data/parser"
--  for_ examplePaths $ \path -> do
--    let name = takeBaseName path
--    bs <- BS.readFile path
--    case runParseGraphQL bs of
--      Left err -> error $ show err
--      Right val ->
--        let p = renderPrettyBS val
--        in case runParseGraphQL p of
--          Left pe -> error $ show pe
--          Right doc -> writeToFile' ("test/data/printer/examples" </> name) doc
--
--
--writeToFile' :: Show a => FilePath -> a -> IO ()
--writeToFile' path actual = BS.writeFile path . BL.toStrict . TEL.encodeUtf8 . pShowNoColor $ actual
--
--readFromFile' :: Read b => FilePath -> IO b
--readFromFile' path = do
--  eVal <- readEither . TL.unpack <$> TLIO.readFile path
--  either throwString pure eVal
