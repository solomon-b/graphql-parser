module GraphQLParser
  ( module T,
    module IR,
    module L,
    module M,
    module P,
    module S,
    runLex,
    runParse,
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

runParse :: B.ByteString -> Either ParseError ExecutableDocument
runParse bs = M.runParser [] bs $ do
  toks <- L.lexer
  P.parser toks
