{
-- We have to disable -XStrictData here, as it doesn't play nicely with Happy.
{-# LANGUAGE NoStrictData #-}
module GraphQLParser.Grammar where

import Control.Monad.State (gets)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict as Map
import Data.Text (Text)
import GraphQLParser.Monad
import GraphQLParser.Token
import GraphQLParser.Span

}

%name parser field
%tokentype { Token }
%monad { Parser }
%error { failure }

%token

'null'      { TokIdentifier (Loc $$ "null" ) }
'"'         { TokSymbol (Loc $$ SymDoubleQuote) }
','         { TokSymbol (Loc $$ SymComma) }
':'         { TokSymbol (Loc $$ SymColon) }
'{'         { TokSymbol (Loc $$ SymCurlyOpen) }
'}'         { TokSymbol (Loc $$ SymCurlyClose) }
'['         { TokSymbol (Loc $$ SymSquareOpen) }
']'         { TokSymbol (Loc $$ SymSquareClose) }
'('         { TokSymbol (Loc $$ SymParenOpen) }
')'         { TokSymbol (Loc $$ SymParenClose) }

int         { TokIntLit $$ }
float       { TokNumLit _ $$ }
bool        { TokBoolLit $$ }
string      { TokStringLit $$ }
ident       { TokIdentifier $$ }
dir         { TokDirective $$ }

%%

name :: { Name }
name : ident { Name (unLoc $1) }

directive :: { Directive }
directive : dir { Directive (Name $ unLoc $1) mempty }
          | dir '(' arguments ')' { Directive (Name $ unLoc $1) $3 }

directives :: { [Directive] }
directives : directive { [$1] }
           | directive directives { $1 : $2 }

field :: { Field }
field : name { Field Nothing $1 mempty mempty mempty }
      | name directives { Field Nothing $1 $2 mempty mempty }
      | name '(' arguments ')' { Field Nothing $1 mempty $3 mempty }
      | name '(' arguments ')' directives { Field Nothing $1 $5 $3 mempty }
      | name '(' arguments ')' selectionSet { Field Nothing $1 mempty $3 $5 }
      | name '(' arguments ')' directives selectionSet { Field Nothing $1 $5 $3 $6 }
      | name selectionSet { Field Nothing $1 mempty mempty $2 }
      | name directives selectionSet { Field Nothing $1 $2 mempty $3 }
      | name ':' name { Field (Just $1) $3 mempty mempty mempty }
      | name ':' name directives { Field (Just $1) $3 $4 mempty mempty }
      | name ':' name '(' arguments ')' { Field (Just $1) $3 mempty $5 mempty }
      | name ':' name '(' arguments ')' directives { Field (Just $1) $3 $7 $5 mempty }
      | name ':' name '(' arguments ')' selectionSet { Field (Just $1) $3 mempty $5 $7 }
      | name ':' name '(' arguments ')' directives selectionSet { Field (Just $1) $3 $7 $5 $8 }
      | name ':' name selectionSet { Field (Just $1) $3 mempty mempty $4 }
      | name ':' name directives selectionSet { Field (Just $1) $3 $4 mempty $5 }

selectionSet :: { SelectionSet }
selectionSet : '{' selections '}' { SelectionSet $2 }

selections :: { [Selection] }
selections : field { [ SelField $1 ] }
           | field selections { SelField $1 : $2 }

arguments :: { HashMap Name Value }
arguments : argument { uncurry Map.singleton $1 }
          | arguments ',' argument { uncurry Map.insert $3 $1 }

argument :: { (Name, Value) }
argument : name ':' value { ($1, $3) }

-- TODO(Solomon): Enum
value :: { Value }
value : 'null' { VNull }
      | '"' string '"' { VString (unLoc $2) }
      | float { VFloat (unLoc $1) }
      | int { VInt (unLoc $1) }
      | bool { VBoolean (unLoc $1) }
      | vlist { $1 }
      | vobject { $1 }
      
vlist :: { Value }
vlist : '[' ']' { VList [] }
      | '[' values ']' { VList $2 }

values :: { [Value] }
values : value { [$1] }
       | value ',' values { $1 : $3 }

vobject :: { Value } 
vobject : '{' '}' { VObject mempty }
        | '{' object '}' { VObject $2 }

object :: { HashMap Name Value }
object : objectField { uncurry Map.singleton $1 }
       | objectField ',' object { uncurry Map.insert $1 $3 }

objectField :: { (Name, Value) }
objectField : name ':' value { ($1, $3) }

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
