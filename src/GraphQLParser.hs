module GraphQLParser
  ( module T,
    module Syntax,
    module L,
    module M,
    module P,
    module S,
    runLex,
    runParseGraphQL,
    runParseExecutable,
    runParseName,
    runParseTypeSystem,
  )
where

import Data.ByteString qualified as B
import GraphQLParser.Grammar as P
import GraphQLParser.Lexer as L
import GraphQLParser.Monad as M
import GraphQLParser.Span as S
import GraphQLParser.Syntax as Syntax
import GraphQLParser.Token as T

runLex :: B.ByteString -> Either ParseError [Token]
runLex bs = M.runParser [] bs L.lexer

runParseGraphQL :: B.ByteString -> Either ParseError GraphQLDocument
runParseGraphQL bs = M.runParser [] bs $ do
  toks <- L.lexer
  P.parseGraphQLDocument toks

runParseExecutable :: B.ByteString -> Either ParseError ExecutableDocument
runParseExecutable bs = M.runParser [] bs $ do
  toks <- L.lexer
  P.parseExecutableDocument toks

runParseTypeSystem :: B.ByteString -> Either ParseError TypeSystemDocument
runParseTypeSystem bs = M.runParser [] bs $ do
  toks <- L.lexer
  P.parseTypeSystemDocument toks

runParseName :: MonadFail m => B.ByteString -> m Name
runParseName bs =
  let result = M.runParser [] bs $ do
        toks <- L.lexer
        P.parseName toks
      errorMessage = show $ bs <> " is not valid GraphQL name"
   in either (\_ -> fail errorMessage) pure result
