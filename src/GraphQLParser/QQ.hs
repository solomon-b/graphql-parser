{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Quasiquotation for 'GraphQLParser.IR' types.
--
-- These quasiquoters can be used to construct GraphQL literal values at
-- compile-time.
module GraphQLParser.QQ
  ( nameQQ,
    documentQQ,
  )
where

-------------------------------------------------------------------------------

import Data.ByteString.Char8 qualified as Char8
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import GraphQLParser (runParseGraphQL, runParseName)
import GraphQLParser.Syntax qualified as Syntax
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (lift)
import Language.Haskell.TH.Syntax.Compat (SpliceQ, examineSplice, liftSplice, unTypeQQuote)

-------------------------------------------------------------------------------

-- | Construct a 'Name' value at compile-time.
litName :: Text -> SpliceQ Syntax.Name
litName txt = liftSplice do
  name' <- runParseName $ TE.encodeUtf8 txt
  examineSplice [||name'||]

-- | Construct 'Syntax.Name' literals at compile-time via quasiquotation.
nameQQ :: QuasiQuoter
nameQQ =
  QuasiQuoter {quoteExp, quotePat, quoteType, quoteDec}
  where
    quotePat _ = error "nameQQ does not support quoting patterns"
    quoteType _ = error "nameQQ does not support quoting types"
    quoteDec _ = error "nameQQ does not support quoting declarations"
    quoteExp = unTypeQQuote . examineSplice . litName . Text.pack

-- | Construct 'Syntax.Document' literals at compile time via
-- quasiquotation.
documentQQ :: QuasiQuoter
documentQQ =
  QuasiQuoter {quoteExp, quotePat, quoteType, quoteDec}
  where
    quotePat _ = error "documentQQ does not support quoting patterns"
    quoteType _ = error "documentQQ does not support quoting types"
    quoteDec _ = error "documentQQ does not support quoting declarations"
    quoteExp s = case runParseGraphQL (Char8.pack s) of
      Left err -> fail $ show err
      Right result -> lift result
