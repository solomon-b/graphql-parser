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
%name parseName name
%tokentype { Token }
%monad { Parser }
%error { failure }

%right LOW
%right '$' '{' '"' '"""'

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
'@'            { TokSymbol (Loc $$ SymAt) }
'$'            { TokSymbol (Loc $$ SymBling) }
'&'            { TokSymbol (Loc $$ SymAmpersand) }
'!'            { TokSymbol (Loc $$ SymBang) }
'"'            { TokSymbol (Loc $$ SymDoubleQuote) }
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

graphqlDocument :: { Document }
graphqlDocument
  : graphqlDefinition { Document (pure $1) }
  | graphqlDefinition graphqlDocument { coerce ($1 : coerce $2)}

executableOrTypeSystemDefiniton :: { Either ExecutableDefinition TypeSystemDefinitionOrExtension }
executableOrTypeSystemDefiniton
  : executableDefinition { Left $1 }
  | typeSystemDefinition { Right (TyDefinition $1) }

--------------------------------------------------------------------------------

graphqlDefinition :: { Definition }
graphqldefinition
  : typeSystemDefinition { DefinitionTypeSystem $1 }
  | executableDefinition { DefinitionExecutable $1 }

--------------------------------------------------------------------------------

-- TODO:
-- typeSystemDefinitionOrExtension :: { TypeSystemDefinitionOrExtension }

typeSystemDefinition :: { TypeSystemDefinition }
typeSystemDefinition
  : schemaDefinition { TypeSystemDefinitionSchema $1 }
  | typeDefinition { TypeSystemDefinitionType $1 }
  | directiveDefinition { TypeSystemDefinitionDirective $1 }

-- TODO:
-- typeSystemExtension :: { TypeSystemExtension }

--------------------------------------------------------------------------------

schemaDefinition :: { SchemaDefinition }
schemaDefinition
  : description 'schema' directives '{' rootOperationTypesDefinition '}' { SchemaDefinition $1 $3 $5 }

rootOperationTypesDefinition :: { RootOperationTypesDefinition }
rootOperationTypesDefinition
  : rootOperationTypesDefinition_ { RootOperationTypesDefinition $1 }

rootOperationTypesDefinition_ :: { NE.NonEmpty RootOperationTypeDefinition }
rootOperationTypesDefinition_
  : rootOperationTypeDefinition { pure $1 }
  | rootOperationTypeDefinition rootOperationTypesDefinition_ { $1 NE.<| $2 }

rootOperationTypeDefinition :: { RootOperationTypeDefinition }
rootOperationTypeDefinition
  : operationType ':' name { RootOperationTypeDefinition $1 $3 }

--------------------------------------------------------------------------------

typeDefinition :: { TypeDefinition }
typedefinition
  : scalarTypeDefinition { STDef $1 }
  | objectTypeDefinition { OTDef $1 }
  | interfaceTypeDefinition { ITDef $1 }
  | unionTypeDefinition { UTDef $1 }
  | enumTypeDefinition { ETDef $1 }
  | inputObjectTypeDefinition { IOTDef $1 }

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

implementsInterfaces :: { Maybe ImplementsInterfaces }
implementsInterfaces
  : 'implements' implementsInterfaces_ { Just (ImplementsInterfaces $2) }
  | { Nothing }

-- TODO: Replace 'concat' with 'snoc'
implementsInterfaces_ :: { NE.NonEmpty Name }
implementsInterfaces_
  : name { pure $1 }
  | '&' name { pure $2 }
  | implementsInterfaces_ '&' name { $1 <> pure $3 }

--------------------------------------------------------------------------------

fieldsDefinition :: { Maybe FieldsDefinition }
fieldsDefinition
  : '{' fieldDefinitions '}' { Just (FieldsDefinition $2) }
  | %prec LOW { Nothing}

fieldDefinitions :: { NE.NonEmpty FieldDefinition }
fieldDefinitions
  : fieldDefinition { pure $1 }
  | fieldDefinition fieldDefinitions { $1 NE.<| $2 }

fieldDefinition :: { FieldDefinition }
fieldDefinition
  : description name argumentsDefinition ':' type directives { FieldDefinition $1 $2 $3 $5 $6 }

--------------------------------------------------------------------------------

unionTypeDefinition :: { UnionTypeDefinition }
unionTypeDefinition
  : description 'union' name directives '=' unionMembers { UnionTypeDefinition $1 $3 $4 $6 } 
  | description 'union' name directives '=' '|' unionMembers { UnionTypeDefinition $1 $3 $4 $7 } 

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
  : enumValueDefinition { [$1] }
  | enumValueDefinition enumValuesDefinition { $1 : $2 }

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
  : description 'directive' '@' dir argumentsDefinition optRepeatable 'on' directiveLocations { DirectiveDefinition $1 (Name $ unLoc $4) $5 $8 }
  | description 'directive' '@' dir argumentsDefinition optRepeatable 'on' '|' directiveLocations { DirectiveDefinition $1 (Name $ unLoc $4) $5 $9 }

directiveLocations :: { [DirectiveLocation] }
directivelocations
  : directiveLocation { [$1] }
  | directiveLocation '|' directiveLocations { $1 : $3 }

directiveLocation :: { DirectiveLocation }
directivelocation
: executableDirectiveLocation { ExecDirLoc $1 }
| typeSystemDirectiveLocation { TypeSysDirLoc $1 }

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

inputFieldsDefinition :: { InputFieldsDefinition}
inputFieldsDefinition
  : '{' inputValuesDefinition '}' { InputFieldsDefinition $2 }
  | %prec LOW { mempty }

argumentsDefinition :: { ArgumentsDefinition }
argumentsDefinition
  : '(' inputValuesDefinition ')' { ArgumentsDefinition $2 }
  | { mempty }

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
  : operationDefinition { ExecutableDefinitionOperation $1}
  | fragmentDefinition { ExecutableDefinitionFragment $1}

operationDefinition :: { OperationDefinition }
operationDefinition
 : operationType directives selectionSet { OperationDefinition $1 Nothing mempty $2 $3 }
 | operationType name directives selectionSet { OperationDefinition $1 (Just $2) mempty $3 $4 }
 | operationType variableDefinitions directives selectionSet { OperationDefinition $1 Nothing (Just $2) $3 $4 }
 | operationType name variableDefinitions directives selectionSet { OperationDefinition  $1 (Just $2) (Just $3) $4 $5 }
 | selectionSet { OperationDefinition OperationTypeQuery Nothing mempty Nothing $1 }

--------------------------------------------------------------------------------

fragmentDefinition :: { FragmentDefinition }
fragmentDefinition
  : 'fragment' name typeCondition directives selectionSet { FragmentDefinition $2 $3 $4 $5 }

fragmentSpread :: { FragmentSpread }
fragmentSpread
  : '...' name directives { FragmentSpread $2 $3 }

inlineFragment :: { InlineFragment }
inlineFragment
  : '...' typeCondition directives selectionSet { InlineFragment (Just $2) $3 $4 }
  | '...' directives selectionSet { InlineFragment Nothing $2 $3 }

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
  : field { SelectionField $1 }
  | fragmentSpread { SelectionFragmentSpread $1 }
  | inlineFragment { SelectionInlineFragment $1 }

--------------------------------------------------------------------------------
-- Input Values

optValue :: { Maybe Value }
optValue
  : value { Just $1 }
  | %prec LOW { Nothing }

values :: { [Value] }
values
  : value { [$1] }
  | value values { $1 : $2 }

value :: { Value }
value
  : 'null' { VNull }
  | stringValue { VString $1 }
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
  | objectField object { uncurry Map.insert $1 $2 }

objectField :: { (Name, Value) }
objectField
  : name ':' value { ($1, $3) }

--------------------------------------------------------------------------------

operationType :: { OperationType }
operationtype
  : 'query' { OperationTypeQuery }
  | 'mutation' { OperationTypeMutation }
  | 'subscription' { OperationTypeSubscription }

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
  | arguments argument { coerce (uncurry Map.insert $2 (coerce $1)) }

argument :: { (Name, Value) }
argument
  : name ':' value { ($1, $3) }
  | '$' name ':' value { ($2, $4) }

variableDefinitions :: { VariablesDefinition }
variabledefinitions
  : '(' variableDefinitions_ ')' { VariablesDefinition $2 }

variableDefinitions_ :: { NE.NonEmpty VariableDefinition }
variabledefinitions_
  : variableDefinition { pure $1 }
  | variableDefinition variableDefinitions_ { $1 NE.<| $2 }

variableDefinition :: { VariableDefinition }
variabledefinition
  : '$' name ':' type optValue directives { VariableDefinition $2 $4 $5 $6 }

typeCondition :: { TypeCondition }
typeCondition
  : 'on' name { TypeCondition $2 }

type :: { Type }
type
  : name { NamedType $1 }
  | type '!' { NonNullType $1 }
  | '[' type ']' { ListType $2 }

directives :: { Maybe Directives }
directives
  : directives_ { Just (Directives $1) }
  | { Nothing }

directives_ :: { NE.NonEmpty Directive }
directives_
  : directive { $1 NE.:| [] }
  | directive directives_ { $1 NE.<| $2 }

directive :: { Directive }
directive
  : '@' dir { Directive (Name $ unLoc $2) Nothing }
  | '@' dir '(' arguments ')' { Directive (Name $ unLoc $2) (Just $4) }

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
