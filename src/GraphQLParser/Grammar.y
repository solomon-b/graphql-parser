{
-- We have to disable -XStrictData here, as it doesn't play nicely with Happy.
{-# LANGUAGE NoStrictData #-}
module GraphQLParser.Grammar where

import Control.Monad.State (gets)
import Data.Coerce
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import GraphQLParser.Monad
import GraphQLParser.Token
import GraphQLParser.Span
import GraphQLParser.Syntax
}

--------------------------------------------------------------------------------

%name parseGraphQLDocument graphqlDocument
%name parseExecutableDocument executableDocument
%name parseTypeSystemDocument typeSystemDocument
%name parseName name
%tokentype { Token }
%monad { Parser }
%error { failure }

%token

--------------------------------------------------------------------------------

-- Reserved Words
'directive'    { TokIdentifier (Loc $$ "directive") }
'enum'         { TokIdentifier (Loc $$ "enum") }
'fragment'     { TokIdentifier (Loc $$ "fragment") }
'implements'   { TokIdentifier (Loc $$ "implements") }
'interface'    { TokIdentifier (Loc $$ "interface") }
'input'        { TokIdentifier (Loc $$ "input") }
'query'        { TokIdentifier (Loc $$ "query") }
'mutation'     { TokIdentifier (Loc $$ "mutation") }
'null'         { TokIdentifier (Loc $$ "null") }
'on'           { TokIdentifier (Loc $$ "on") }
'repeatable'   { TokIdentifier (Loc $$ "repeatable") }
'scalar'       { TokIdentifier (Loc $$ "scalar") }
'schema'       { TokIdentifier (Loc $$ "schema") }
'subscription' { TokIdentifier (Loc $$ "subscription") }
'type'         { TokIdentifier (Loc $$ "type") }
'union'        { TokIdentifier (Loc $$ "union") }

-- Executable Directive Locations
'QUERY'               { TokIdentifier (Loc $$ "QUERY") }
'MUTATION'            { TokIdentifier (Loc $$ "MUTATION") }
'SUBSCRIPTION'        { TokIdentifier (Loc $$ "SUBSCRIPTION") }
'FIELD'               { TokIdentifier (Loc $$ "FIELD") }
'FRAGMENT_DEFINITION' { TokIdentifier (Loc $$ "FRAGMENT_DEFINITION") }
'FRAGMENT_SPREAD'     { TokIdentifier (Loc $$ "FRAGMENT_SPREAD") }
'INLINE_FRAGMENT'     { TokIdentifier (Loc $$ "INLINE_FRAGMENT") }
'VARIABLE_DEFINITION' { TokIdentifier (Loc $$ "VARIABLE_DEFINITION") }

-- Type System Directive Locations
'SCHEMA'                 { TokIdentifier (Loc $$ "SCHEMA") }
'SCALAR'                 { TokIdentifier (Loc $$ "SCALAR") }
'OBJECT'                 { TokIdentifier (Loc $$ "OBJECT") }
'FIELD_DEFINITION'       { TokIdentifier (Loc $$ "FIELD_DEFINITION") }
'ARGUMENT_DEFINITION'    { TokIdentifier (Loc $$ "ARGUMENT_DEFINITION") }
'INTERFACE'              { TokIdentifier (Loc $$ "INTERFACE") }
'UNION'                  { TokIdentifier (Loc $$ "UNION") }
'ENUM'                   { TokIdentifier (Loc $$ "ENUM") }
'ENUM_VALUE'             { TokIdentifier (Loc $$ "ENUM_VALUE") }
'INPUT_OBJECT'           { TokIdentifier (Loc $$ "INPUT_OBJECT") }
'INPUT_FIELD_DEFINITION' { TokIdentifier (Loc $$ "INPUT_FIELD_DEFINITION") }

-- Symbols
'$'            { TokSymbol (Loc $$ SymBling) }
'&'            { TokSymbol (Loc $$ SymAmpersand) }
'!'            { TokSymbol (Loc $$ SymBang) }
'"'            { TokSymbol (Loc $$ SymDoubleQuote) }
','            { TokSymbol (Loc $$ SymComma) }
':'            { TokSymbol (Loc $$ SymColon) }
'='            { TokSymbol (Loc $$ SymEq) }
'|'            { TokSymbol (Loc $$ SymPipe) }
'{'            { TokSymbol (Loc $$ SymCurlyOpen) }
'}'            { TokSymbol (Loc $$ SymCurlyClose) }
'['            { TokSymbol (Loc $$ SymSquareOpen) }
']'            { TokSymbol (Loc $$ SymSquareClose) }
'('            { TokSymbol (Loc $$ SymParenOpen) }
')'            { TokSymbol (Loc $$ SymParenClose) }
'...'          { TokSymbol (Loc $$ SymSpread) }
'"""'          { TokSymbol (Loc $$ SymBlockQuote) }

-- Scalars
int            { TokIntLit $$ }
float          { TokNumLit _ $$ }
bool           { TokBoolLit $$ }
blockString    { TokStringBlock $$ }
string         { TokStringLit $$ }
ident          { TokIdentifier $$ }
dir            { TokDirective $$ }

%%

--------------------------------------------------------------------------------

graphqlDocument :: { GraphQLDocument }
graphqlDocument
  : executableOrTypeSystemDefiniton { Document (pure $1) }
  | executableOrTypeSystemDefiniton graphqlDocument { coerce ($1 : coerce @_ @[Either ExecutableDefinition TypeSystemDefinitionOrExtension] $2)}

executableOrTypeSystemDefiniton :: { Either ExecutableDefinition TypeSystemDefinitionOrExtension }
executableOrTypeSystemDefiniton
  : executableDefinition { Left $1 }
  | typeSystemDefinition { Right (Left $1) }

executableDocument :: { ExecutableDocument }
executabledocument
  : executableDefinition { Document (pure $1) }
  | executableDefinition executableDocument { coerce ($1 : coerce $2)}

typeSystemDocument :: { TypeSystemDocument }
typesystemdocument
  : typeSystemDefinition { Document (pure $ Left $1) }
  | typeSystemDefinition typeSystemDocument { coerce (Left $1 : coerce @_ @[TypeSystemDefinitionOrExtension] $2) }

--------------------------------------------------------------------------------

-- TODO:
-- typeSystemDefinitionOrExtension :: { TypeSystemDefinitionOrExtension }

typeSystemDefinition :: { TypeSystemDefinition }
typeSystemDefinition
  : schemaDefinition { Left $ Left $1 }
  | typeDefinition { Left $ Right $1 }
  | directiveDefinition { Right $1 }

-- TODO:
-- typeSystemExtension :: { TypeSystemExtension }

--------------------------------------------------------------------------------

schemaDefinition :: { SchemaDefinition }
schemaDefinition
  : description 'schema' directives '{' rootOperationTypeDefinitions '}' { SchemaDefinition $1 $3 $5 }

rootOperationTypeDefinitions :: { [RootOperationTypeDefinition] }
rootOperationTypeDefinitions
  : rootOperationTypeDefinition { [$1] }
  | rootOperationTypeDefinition rootOperationTypeDefinitions { $1 : $2 }

rootOperationTypeDefinition :: { RootOperationTypeDefinition }
rootOperationTypeDefinition
  : operationType ':' name { RootOperationTypeDefinition $1 $3 }

--------------------------------------------------------------------------------

typeDefinition :: { TypeDefinition }
typedefinition
  : scalarTypeDefinition { Left $ Left $ Left $ Left $ Left $1 }
  | objectTypeDefinition { Left $ Left $ Left $ Left $ Right $1 }
  | interfaceTypeDefinition { Left $ Left $ Left $ Right $1 }
  | unionTypeDefinition { Left $ Left $ Right $1 }
  | enumTypeDefinition { Left $ Right $1 }
  | inputObjectTypeDefinition { Right $1 }

--------------------------------------------------------------------------------

scalarTypeDefinition :: { ScalarTypeDefinition }
scalarTypeDefinition
  : description 'scalar' name directives { ScalarTypeDefinition $1 $3 $4 } 

--------------------------------------------------------------------------------

objectTypeDefinition :: { ObjectTypeDefinition }
objectTypeDefinition
  : description 'type' name implementsInterfaces directives fieldsDefinition { ObjectTypeDefinition $1 $3 $4 $5 $6 }

--------------------------------------------------------------------------------

interfaceTypeDefinition :: { InterfaceTypeDefinition }
interfaceTypeDefinition
  : description 'interface' name implementsInterfaces directives fieldsDefinition { InterfaceTypeDefinition $1 $3 $4 $5 $6 }

-- TODO: Replace 'concat' with 'cons'
implementsInterfaces :: { [Name] }
implementsInterfaces
  : implementsInterfaces '&' name { $1 <> [$3] }
  | 'implements' name { [$2] }

--------------------------------------------------------------------------------

fieldsDefinition :: { [FieldDefinition] }
fieldsDefinition
  : '{' fieldDefinitions '}' { $2 }

fieldDefinitions :: { [FieldDefinition] }
fieldDefinitions
  : fieldDefinition { [$1] }
  | fieldDefinition fieldDefinitions { $1 : $2 }

fieldDefinition :: { FieldDefinition }
fieldDefinition
  : description name argumentsDefinition ':' type directives { FieldDefinition $1 $2 $3 $5 $6 }

--------------------------------------------------------------------------------

unionTypeDefinition :: { UnionTypeDefinition }
unionTypeDefinition
  : description 'union' name directives '=' unionMembers { UnionTypeDefinition $1 $3 $4 $6 } 

unionMembers :: { [Name] }
unionMembers
  : name { [$1] }
  | name '|' unionMembers { $1 : $3 }

--------------------------------------------------------------------------------

enumTypeDefinition :: { EnumTypeDefinition }
enumTypeDefinition
: description 'enum' name directives '{' enumValuesDefinition '}' { EnumTypeDefinition $1 $3 $4 $6 }

enumValuesDefinition :: { [EnumValueDefinition] }
enumValuesDefinition
  : enumValueDefinition enumValuesDefinition { $1 : $2 }

enumValueDefinition :: { EnumValueDefinition }
enumValueDefinition
  : description name directives { EnumValueDefinition $1 $2 $3 } 

--------------------------------------------------------------------------------

inputObjectTypeDefinition :: { InputObjectTypeDefinition }
inputObjectTypeDefinition
  : description 'input' name directives inputFieldsDefinition { InputObjectTypeDefinition $1 $3 $4 $5 }

--------------------------------------------------------------------------------

directiveDefinition :: { DirectiveDefinition }
directivedefinition
  : description 'directive' dir argumentsDefinition optRepeatable 'on' directiveLocations { DirectiveDefinition $1 (Name $ unLoc $3) $4 $7 }

directiveLocations :: { [DirectiveLocation] }
directivelocations
  : directiveLocation { [$1] }
  | directiveLocation '|' directiveLocations { $1 : $3 }

directiveLocation :: { DirectiveLocation }
directivelocation
: executableDirectiveLocation { Left $1 }
| typeSystemDirectiveLocation { Right $1 }

executableDirectiveLocation :: { ExecutableDirectiveLocation }
executableDirectiveLocation
  : 'QUERY' { EDLQUERY }
  | 'MUTATION' { EDLMUTATION }
  | 'SUBSCRIPTION' { EDLSUBSCRIPTION }
  | 'FIELD' { EDLFIELD }
  | 'FRAGMENT_DEFINITION' { EDLFRAGMENT_DEFINITION }
  | 'FRAGMENT_SPREAD' { EDLFRAGMENT_SPREAD }
  | 'INLINE_FRAGMENT' { EDLINLINE_FRAGMENT }
  | 'VARIABLE_DEFINITION' { EDLVARIABLE_DEFINITION }

typeSystemDirectiveLocation :: { TypeSystemDirectiveLocation }
typeSystemDirectiveLocation
: 'SCHEMA' { TSDLSCHEMA }
| 'SCALAR' { TSDLSCALAR }
| 'OBJECT' { TSDLOBJECT }
| 'FIELD_DEFINITION' { TSDLFIELD_DEFINITION }
| 'ARGUMENT_DEFINITION' { TSDLARGUMENT_DEFINITION }
| 'INTERFACE' { TSDLINTERFACE }
| 'UNION' { TSDLUNION }
| 'ENUM' { TSDLENUM }
| 'ENUM_VALUE' { TSDLENUM_VALUE }
| 'INPUT_OBJECT' { TSDLINPUT_OBJECT }
| 'INPUT_FIELD_DEFINITION' { TSDLINPUT_FIELD_DEFINITION }

--------------------------------------------------------------------------------

inputFieldsDefinition :: { [InputValueDefinition] }
inputFieldsDefinition
  : '{' inputValuesDefinition '}' { $2 }

argumentsDefinition :: { [InputValueDefinition] }
argumentsDefinition
  : '(' inputValuesDefinition ')' { $2 }

inputValuesDefinition :: { [InputValueDefinition] }
inputValuesDefinition
  : inputValueDefinition { [$1] }
  | inputValueDefinition inputValuesDefinition { $1 : $2 }

inputValueDefinition :: { InputValueDefinition }
inputValueDefinition
  : description name ':' type optValue directives { InputValueDefinition $1 $2 $4 $5 $6 }

--------------------------------------------------------------------------------

executableDefinition :: { ExecutableDefinition }
executabledefinition
  : operationDefinition { Left $1}
  | fragmentDefinition { Right $1}

operationDefinition :: { OperationDefinition }
operationDefinition
 : operationType directives selectionSet { OperationDefinition  $1 Nothing [] $2 $3 }
 | operationType name directives selectionSet { OperationDefinition  $1 (Just $2) [] $3 $4 }
 | operationType variableDefinitions directives selectionSet { OperationDefinition  $1 Nothing $2 $3 $4 }
 | operationType name variableDefinitions directives selectionSet { OperationDefinition  $1 (Just $2) $3 $4 $5 }
 | selectionSet { OperationDefinition Query Nothing [] [] $1 }

--------------------------------------------------------------------------------

fragmentDefinition :: { FragmentDefinition }
fragmentDefinition
  : 'fragment' fragmentName 'on' type directives selectionSet { FragmentDefinition $2 $4 $5 $6 }

fragmentSpread :: { FragmentSpread }
fragmentSpread
  : '...' fragmentName directives { FragmentSpread $2 $3 }

inlineFragment :: { InlineFragment }
inlineFragment
  : '...' 'on' name directives selectionSet { InlineFragment (Just $3) $4 $5 }
  | '...' directives selectionSet { InlineFragment Nothing $2 $3 }

-- TODO: Fail if `Name` == `on`
fragmentName :: { FragmentName }
fragmentname
  : name { FragmentName $1 }

--------------------------------------------------------------------------------

field :: { Field }
field
  : aliasAndName directives { Field (fst $1) (snd $1) mempty $2 mempty }
  | aliasAndName '(' arguments ')' directives { Field (fst $1) (snd $1) $3 $5 mempty }
  | aliasAndName '(' arguments ')' directives selectionSet { Field (fst $1) (snd $1) $3 $5 (Just $6) }
  | aliasAndName directives selectionSet { Field (fst $1) (snd $1) mempty $2 (Just $3) }

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
-- Input Values

optValue :: { Maybe Value }
optValue
  : value { Just $1 }
  | { Nothing }

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

stringValue :: { Text }
stringValue
  : '"' '"' { "" }
  | '"' string '"' { unLoc $2 }
  | '"""' blockString '"""' { unLoc $2 }
      
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

aliasAndName :: { (Maybe Name, Name) }
aliasAndName
  : name ':' name { (Just $1, $3) }
  | name { (Nothing, $1) }

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
  : '$' name ':' type optValue directives { VariableDefinition $2 $4 $5 $6 }

type :: { Type }
type
  : name { NamedType $1 }
  | type '!' { NonNullType $1 }
  | '[' type ']' { ListType $2 }

directives :: { [Directive] }
directives
  : directives_ { $1 }
  | { [] }

directives_ :: { [Directive] }
directives_
  : directive { [$1] }
  | directive directives_ { $1 : $2 }

directive :: { Directive }
directive
  : dir { Directive (Name $ unLoc $1) mempty }
  | dir '(' arguments ')' { Directive (Name $ unLoc $1) $3 }

description :: { Maybe Description }
description
  : stringValue { Just (Description $1) }
  | { Nothing }

optRepeatable :: { Maybe Span }
optRepeatable
: 'repeatable' { Just $1 }
| { Nothing }

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
