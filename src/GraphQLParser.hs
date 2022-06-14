module GraphQLParser
  ( module T,
    module Syntax,
    module L,
    module M,
    module P,
    module S,
    runLex,
    runParseGraphQL,
    runParseName,
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

runParseGraphQL :: B.ByteString -> Either ParseError Document
runParseGraphQL bs = M.runParser [] bs $ do
  toks <- L.lexer
  P.parseGraphQLDocument toks

runParseName :: MonadFail m => B.ByteString -> m Name
runParseName bs =
  let result = M.runParser [] bs $ do
        toks <- L.lexer
        P.parseName toks
      errorMessage = show $ bs <> " is not valid GraphQL name"
   in either (\_ -> fail errorMessage) pure result
