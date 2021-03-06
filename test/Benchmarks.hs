import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Traversable (for)
import GraphQLParser
import GraphQLParser.Generator
import Test.Tasty.Bench (bench, bgroup, defaultMain, nf, whnf)

-------------------------------------------------------------------------------

genDocs :: Int -> IO [(Int, Document)]
genDocs num =
  for [1 .. num] $ \n -> (n,) <$> generate genDocument

genTexts :: Int -> IO [(Int, [Text])]
genTexts num =
  for [1 .. num] $ \n -> do
    texts <- for [1 .. 500 :: Int] . const $ generate genText
    pure (n, texts)

main :: IO ()
main = do
  docs <- genDocs 10
  texts <- genTexts 10
  let grp5 = mkPGrp renderedDocs
      grp6 = mkNGrp texts
      renderedDocs = map (\(n, q) -> (n, renderPrettyBS q)) docs
  defaultMain [grp5, grp6]
  where
    mkNGrp texts =
      bgroup "checking name validity" $
        texts <&> \(n, t) ->
          bench (show n) $ nf (length . mapMaybe mkName) t

    mkPGrp qs =
      bgroup "parsing executableDocument" $
        map (\(n, q) -> bench (show n) $ whnf runParseGraphQL q) qs
