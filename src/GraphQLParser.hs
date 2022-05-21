module GraphQLParser
  ( module T,
    module IR,
    module L,
    module M,
    module P,
    module S,
    runLex,
    runParseExecutable,
    runParseTypeSystem,
  )
where

import Data.ByteString qualified as B
import GraphQLParser.Grammar as P
import GraphQLParser.IR as IR
import GraphQLParser.Lexer as L
import GraphQLParser.Monad as M
import GraphQLParser.Span as S
import GraphQLParser.Token as T

runLex :: B.ByteString -> Either ParseError [Token]
runLex bs = M.runParser [] bs L.lexer

runParseExecutable :: B.ByteString -> Either ParseError ExecutableDocument
runParseExecutable bs = M.runParser [] bs $ do
  toks <- L.lexer
  P.parseExecutableDocument toks

runParseTypeSystem :: B.ByteString -> Either ParseError TypeSystemDocument 
runParseTypeSystem bs = M.runParser [] bs $ do
  toks <- L.lexer
  P.parseTypeSystemDocument toks
