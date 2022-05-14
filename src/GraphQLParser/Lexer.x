{
module GraphQLParser.Lexer where

import Control.Monad.State (gets)
import Data.Text qualified as T
import GraphQLParser.Monad
import GraphQLParser.Span
import GraphQLParser.Token
}

$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [a-zA-Z0-9]
$hex = [A-Fa-f0-9]

tokens :-

-- Whitespace insensitive
<0> $white+                 ;

{
-- | The monadic wrapper for 'alexScan'. The 'Parser' type is defined
-- in 'GraphQLParser.Monad' as a transformer stack of 'StateT' and
-- 'Except'. The Start Code Stack, current 'Span', and 'AlexInput' are
-- tracked in 'StateT'.
--
-- 'scan' recursively consumes the consumes the 'AlexInput' from state
-- until it produces a 'Token', an error, or reaches the end of the
-- file.
scan :: Parser Token
scan = do
  input <- getInput
  code <- startCode
  src <- gets parseSource
  case alexScan input code of
    AlexEOF -> pure EOF
    AlexError (AlexInput pos _ _ _) ->
      parseError $ InvalidLexeme pos src
    AlexSkip rest _ -> do
      advance rest
      scan
    AlexToken rest nbytes action -> do
      advance rest
      action (slice nbytes input)

-- | The entry point to the lexer. Recursively calls 'scan' to yield
-- tokens unti we hit EOF.
lexer :: Parser [Token]
lexer = do
  tok <- scan
  case tok of
    EOF -> pure []
    x -> (x :) <$> lexer
}
