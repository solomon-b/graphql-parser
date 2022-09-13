module GraphQLParser
  ( module T,
    module Syntax,
    module M,
    module S,
    module E,
    parseGraphQL,
    parseExecutableDocument,
    parseName,
  )
where

import Data.ByteString qualified as B
import GraphQLParser.Error as E
import GraphQLParser.Grammar qualified as P
import GraphQLParser.Lexer qualified as L
import GraphQLParser.Monad as M
import GraphQLParser.Span as S
import GraphQLParser.Syntax as Syntax
import GraphQLParser.Token as T

parseGraphQL :: B.ByteString -> Either ParseError Document
parseGraphQL bs = M.runParser [] bs $ do
  toks <- L.lexer
  P.parseGraphQLDocument toks

parseExecutableDocument :: B.ByteString -> Either ParseError ExecutableDefinition
parseExecutableDocument bs = M.runParser [] bs $ do
  toks <- L.lexer
  P.parseExecutableDocument toks

parseName :: MonadFail m => B.ByteString -> m Name
parseName bs =
  let result = M.runParser [] bs $ do
        toks <- L.lexer
        fmap unLoc $ P.parseName toks
      errorMessage = show $ bs <> " is not valid GraphQL name"
   in either (\_ -> fail errorMessage) pure result
