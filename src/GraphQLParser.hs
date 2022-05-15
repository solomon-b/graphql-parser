module GraphQLParser
  ( module T,
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
import GraphQLParser.Lexer as L
import GraphQLParser.Monad as M
import GraphQLParser.Token as T
import GraphQLParser.Span as S

runLex :: B.ByteString -> Either ParseError [Token]
runLex bs = M.runParser [] bs L.lexer

runParse :: B.ByteString -> Either ParseError SelectionSet
runParse bs = M.runParser [] bs $ do
  toks <- L.lexer
  P.parser toks
