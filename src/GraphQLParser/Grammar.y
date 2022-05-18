{
-- We have to disable -XStrictData here, as it doesn't play nicely with Happy.
{-# LANGUAGE NoStrictData #-}
module GraphQLParser.Grammar where

import Control.Monad.State (gets)
import Data.Coerce
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict as Map
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import GraphQLParser.Monad
import GraphQLParser.IR
import GraphQLParser.Token
import GraphQLParser.Span
}

--------------------------------------------------------------------------------

%name parser executableDocument
%tokentype { Token }
%monad { Parser }
%error { failure }

%token

--------------------------------------------------------------------------------

'query'        { TokIdentifier (Loc $$ "query") }
'mutation'     { TokIdentifier (Loc $$ "mutation") }
'subscription' { TokIdentifier (Loc $$ "subscription") }
'null'         { TokIdentifier (Loc $$ "null") }
'on'           { TokIdentifier (Loc $$ "on") }
'fragment'     { TokIdentifier (Loc $$ "fragment") }
'$'            { TokSymbol (Loc $$ SymBling) }
'!'            { TokSymbol (Loc $$ SymBang) }
'"'            { TokSymbol (Loc $$ SymDoubleQuote) }
','            { TokSymbol (Loc $$ SymComma) }
':'            { TokSymbol (Loc $$ SymColon) }
'{'            { TokSymbol (Loc $$ SymCurlyOpen) }
'}'            { TokSymbol (Loc $$ SymCurlyClose) }
'['            { TokSymbol (Loc $$ SymSquareOpen) }
']'            { TokSymbol (Loc $$ SymSquareClose) }
'('            { TokSymbol (Loc $$ SymParenOpen) }
')'            { TokSymbol (Loc $$ SymParenClose) }
'...'          { TokSymbol (Loc $$ SymSpread) }

int            { TokIntLit $$ }
float          { TokNumLit _ $$ }
bool           { TokBoolLit $$ }
string         { TokStringLit $$ }
ident          { TokIdentifier $$ }
dir            { TokDirective $$ }

%%

--------------------------------------------------------------------------------

executableDocument :: { ExecutableDocument }
executabledocument
  : executableDefinition { Document (pure $1) }
  | executableDefinition executableDocument { coerce ($1 : coerce $2)}

-- TODO:
-- typeSystemDocument :: { TypeSystemDocument }

--------------------------------------------------------------------------------

-- TODO:
-- typeSystemDefinitionOrExtension :: { TypeSystemDefinitionOrExtension }

-- TODO:
-- typeSystemDefinition :: { TypeSystemDefinition }

-- TODO:
-- typeSystemExtension :: { TypeSystemExtension }

--------------------------------------------------------------------------------

executableDefinition :: { ExecutableDefinition }
executabledefinition
  : operationDefinition { Left $1}
  | fragmentDefinition { Right $1}

operationDefinition :: { OperationDefinition }
operationDefinition
 : operationType selectionSet { OperationDefinition $1 Nothing [] [] $2 }
 | operationType name selectionSet { OperationDefinition $1 (Just $2) [] [] $3 }
 | operationType variableDefinitions selectionSet { OperationDefinition  $1 Nothing $2 [] $3 }
 | operationType directives selectionSet { OperationDefinition  $1 Nothing [] $2 $3 }
 | operationType name variableDefinitions selectionSet { OperationDefinition  $1 (Just $2) $3 [] $4 }
 | operationType name directives selectionSet { OperationDefinition  $1 (Just $2) [] $3 $4 }
 | operationType variableDefinitions directives selectionSet { OperationDefinition  $1 Nothing $2 $3 $4 }
 | operationType name variableDefinitions directives selectionSet { OperationDefinition  $1 (Just $2) $3 $4 $5 }
 | selectionSet { OperationDefinition Query Nothing [] [] $1 }

--------------------------------------------------------------------------------

fragmentDefinition :: { FragmentDefinition }
fragmentDefinition
  : 'fragment' fragmentName 'on' type directives selectionSet { FragmentDefinition $2 $4 $5 $6 }
  | 'fragment' fragmentName 'on' type selectionSet { FragmentDefinition $2 $4 mempty $5 }

fragmentSpread :: { FragmentSpread }
fragmentSpread
  : '...' fragmentName directives { FragmentSpread $2 $3 }
  | '...' fragmentName { FragmentSpread $2 [] }

inlineFragment :: { InlineFragment }
inlineFragment
  : '...' 'on' name directives selectionSet { InlineFragment (Just $3) $4 $5 }
  | '...' 'on' name selectionSet { InlineFragment (Just $3) mempty $4 }
  | '...' directives selectionSet { InlineFragment Nothing $2 $3 }
  | '...' selectionSet { InlineFragment Nothing mempty $2 }

-- TODO: Fail if `Name` == `on`
fragmentName :: { FragmentName }
fragmentname
  : name { FragmentName $1 }

--------------------------------------------------------------------------------

field :: { Field }
field
  : name { Field Nothing $1 mempty mempty mempty }
  | name directives { Field Nothing $1 mempty $2 mempty }
  | name '(' arguments ')' { Field Nothing $1 $3 mempty mempty }
  | name '(' arguments ')' directives { Field Nothing $1 $3 $5 mempty }
  | name '(' arguments ')' selectionSet { Field Nothing $1 $3 mempty (Just $5) }
  | name '(' arguments ')' directives selectionSet { Field Nothing $1 $3 $5 (Just $6) }
  | name selectionSet { Field Nothing $1 mempty mempty (Just $2) }
  | name directives selectionSet { Field Nothing $1 mempty $2 (Just $3) }
  | name ':' name { Field (Just $1) $3 mempty mempty mempty }
  | name ':' name directives { Field (Just $1) $3 mempty $4 mempty }
  | name ':' name '(' arguments ')' { Field (Just $1) $3 $5 mempty mempty }
  | name ':' name '(' arguments ')' directives { Field (Just $1) $3 $5 $7 mempty }
  | name ':' name '(' arguments ')' selectionSet { Field (Just $1) $3 $5 mempty (Just $7) }
  | name ':' name '(' arguments ')' directives selectionSet { Field (Just $1) $3 $5 $7 (Just $8) }
  | name ':' name selectionSet { Field (Just $1) $3 mempty mempty (Just $4) }
  | name ':' name directives selectionSet { Field (Just $1) $3 mempty $4 (Just $5) }

--------------------------------------------------------------------------------

selectionSet :: { SelectionSet }
selectionSet
  : '{' selections '}' { SelectionSet (NE.fromList $2) }

selections :: { [Selection] }
selections
  : selection { [ $1 ] }
  | selection selections { $1 : $2 }

selection :: { Selection }
selection
  : field { Left (Left $1) }
  | fragmentSpread { Left (Right $1) }
  | inlineFragment { Right $1 }

--------------------------------------------------------------------------------

values :: { [Value] }
values
  : value { [$1] }
  | value ',' values { $1 : $3 }

value :: { Value }
value
  : 'null' { VNull }
  | '"' string '"' { VString (unLoc $2) }
  | float { VFloat (unLoc $1) }
  | int { VInt (unLoc $1) }
  | bool { VBoolean (unLoc $1) }
  | vlist { $1 }
  | vobject { $1 }
  | '$' ident { VVar (unLoc $2) }
      
vlist :: { Value }
vlist
  : '[' ']' { VList [] }
  | '[' values ']' { VList $2 }

vobject :: { Value } 
vobject
  : '{' '}' { VObject mempty }
  | '{' object '}' { VObject $2 }

object :: { HashMap Name Value }
object
  : objectField { uncurry Map.singleton $1 }
  | objectField ',' object { uncurry Map.insert $1 $3 }

objectField :: { (Name, Value) }
objectField
  : name ':' value { ($1, $3) }

--------------------------------------------------------------------------------

operationType :: { OperationType }
operationtype
  : 'query' { Query }
  | 'mutation' { Mutation }
  | 'subscription' { Subscription }

name :: { Name }
name
  : ident { Name (unLoc $1) }

arguments :: { Arguments }
arguments
  : argument { Arguments (uncurry Map.singleton $1) }
  | arguments ',' argument { coerce (uncurry Map.insert $3 (coerce $1)) }

argument :: { (Name, Value) }
argument
  : name ':' value { ($1, $3) }
  | '$' name ':' value { ($2, $4) }

variableDefinitions :: { [VariableDefinition] }
variabledefinitions
  : '(' variableDefinitions_ ')' { $2 }

variableDefinitions_ :: { [VariableDefinition] }
variabledefinitions_
  : variableDefinition { [ $1 ] }
  | variableDefinition ',' variableDefinitions { $1 : $3 }

variableDefinition :: { VariableDefinition }
variabledefinition
  : '$' name ':' type { VariableDefinition $2 $4 Nothing mempty }
  | '$' name ':' type value { VariableDefinition $2 $4 (Just $5) mempty }
  | '$' name ':' type value directives { VariableDefinition $2 $4 (Just $5) $6 }

type :: { Type }
type
  : name { NamedType $1 }
  | type '!' { NonNullType $1 }
  | '[' type ']' { ListType $2 }

directives :: { [Directive] }
directives
  : directive { [$1] }
  | directive directives { $1 : $2 }

directive :: { Directive }
directive
  : dir { Directive (Name $ unLoc $1) mempty }
  | dir '(' arguments ')' { Directive (Name $ unLoc $1) $3 }

--------------------------------------------------------------------------------

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
