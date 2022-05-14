{
-- We have to disable -XStrictData here, as it doesn't play nicely with Happy.
{-# LANGUAGE NoStrictData #-}
module GraphQLParser.Grammar where

import Control.Monad.State (gets)
import GraphQLParser.Monad
import GraphQLParser.Token
import GraphQLParser.Spans

}

%name parser expr
%tokentype { Token }
%monad { Parser }
%error { failure }

%token
number      { TokNumLit _ $$ }
int         { TokIntLit _ $$ }
'true'      { TokBoolLit $$ }
'false'     { TokBoolLit $$ }

string      { TokStringLit $$ }
ident       { TokIdentifier $$ }

':'         { TokSymbol (Loc $$ SymColon) }
'{'         { TokSymbol (Loc $$ SymCurlyOpen) }
'}'         { TokSymbol (Loc $$ SymCurlyClose) }
'('         { TokSymbol (Loc $$ SymParenOpen) }
')'         { TokSymbol (Loc $$ SymParenClose) }

%%

expr :: { GraphQL }
expr : _ { GraphQL}

{
failure :: [Token] -> Parser a
failure [] = do
  sp <- location
  src <- gets parseSource
  parseError $ EmptyTokenStream sp src
failure (tok:_) = do
  sp <- location
  src <- gets parseSource
  -- TODO: fix source position capture here. I think we need the prior span.
  parseError $ UnexpectedToken (Loc sp tok) src
}
