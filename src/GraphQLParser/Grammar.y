{
-- We have to disable -XStrictData here, as it doesn't play nicely with Happy.
{-# LANGUAGE NoStrictData #-}
module GraphQLParser.Grammar where

import Control.Monad.State (gets)
import Data.Coerce
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GraphQLParser.Error
import GraphQLParser.Monad
import GraphQLParser.Span
import GraphQLParser.Syntax
import GraphQLParser.Token
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
  : description 'schema' directives '{' rootOperationTypesDefinition '}'
      { SchemaDefinition ((fromMaybe $2 (fmap locate $1)) <> locate $6) (fmap unLoc $1) (fmap unLoc $3) $5 }

rootOperationTypesDefinition :: { RootOperationTypesDefinition }
rootOperationTypesDefinition
  : rootOperationTypesDefinition_ { RootOperationTypesDefinition $1 }

rootOperationTypesDefinition_ :: { NE.NonEmpty RootOperationTypeDefinition }
rootOperationTypesDefinition_
  : rootOperationTypeDefinition { pure $1 }
  | rootOperationTypeDefinition rootOperationTypesDefinition_ { $1 NE.<| $2 }

rootOperationTypeDefinition :: { RootOperationTypeDefinition }
rootOperationTypeDefinition
  : operationType ':' name { RootOperationTypeDefinition (locate $1 <> locate $3) (unLoc $1) (unLoc $3) }

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
  : description 'scalar' name directives { ScalarTypeDefinition (fromMaybe (locate $2) (fmap locate $1) <> maybeLoc $3 $4) (fmap unLoc $1) (unLoc $3) (fmap unLoc $4) } 

--------------------------------------------------------------------------------

objectTypeDefinition :: { ObjectTypeDefinition }
objectTypeDefinition
  : description 'type' name implementsInterfaces directives fieldsDefinition
      { ObjectTypeDefinition (fromMaybe (locate $2) (fmap locate $1) <> maybeLoc (maybeLoc (maybeLoc $3 $4) $5) $6) (fmap unLoc $1) (unLoc $3) $4 (fmap unLoc $5) $6 }

--------------------------------------------------------------------------------

interfaceTypeDefinition :: { InterfaceTypeDefinition }
interfaceTypeDefinition
  : description 'interface' name implementsInterfaces directives fieldsDefinition
      { InterfaceTypeDefinition (fromMaybe (locate $2) (fmap locate $1) <> maybeLoc (maybeLoc (maybeLoc $3 $4) $5) $6) (fmap unLoc $1) (unLoc $3) $4 (fmap unLoc $5) $6 }

implementsInterfaces :: { Maybe ImplementsInterfaces }
implementsInterfaces
  : implementsInterfaces_ { Just (ImplementsInterfaces (locate $1) (unLoc $1)) }
  | { Nothing }

implementsInterfaces_ :: { Loc (NE.NonEmpty Name) }
implementsInterfaces_
  : implementsInterfaces_ '&' name { Loc (locate $1 <> locate $3) (unLoc $1 <> pure (unLoc $3)) }
  | 'implements' '&' name { Loc (locate $1 <> locate $3) (pure (unLoc $3)) }
  | 'implements' name { Loc (locate $1 <> locate $2) (pure (unLoc $2)) }

--------------------------------------------------------------------------------

fieldsDefinition :: { Maybe FieldsDefinition }
fieldsDefinition
  : '{' fieldDefinitions '}' { Just (FieldsDefinition (locate $1 <> locate $3) $2) }
  | %prec LOW { Nothing }

fieldDefinitions :: { NE.NonEmpty FieldDefinition }
fieldDefinitions
  : fieldDefinition { pure $1 }
  | fieldDefinition fieldDefinitions { $1 NE.<| $2 }

fieldDefinition :: { FieldDefinition }
fieldDefinition
  : description name argumentsDefinition ':' type directives
      { FieldDefinition (fromMaybe (locate $2) (fmap locate $1) <> maybeLoc $5 $6) (fmap unLoc $1) (unLoc $2) $3 $5 (fmap unLoc $6) }

--------------------------------------------------------------------------------

unionTypeDefinition :: { UnionTypeDefinition }
unionTypeDefinition
  : description 'union' name directives '=' unionMembers
      { UnionTypeDefinition (fromMaybe (locate $2) (fmap locate $1) <> locate $6) (fmap unLoc $1) (unLoc $3) (fmap unLoc $4) (unLoc $6) } 
  | description 'union' name directives '=' '|' unionMembers
      { UnionTypeDefinition (fromMaybe (locate $2) (fmap locate $1) <> locate $7) (fmap unLoc $1) (unLoc $3) (fmap unLoc $4) (unLoc $7) } 

unionMembers :: { Loc [Name] }
unionMembers
  : name { Loc (locate $1) [unLoc $1] }
  | name '|' unionMembers { Loc (locate $1 <> locate $3) (unLoc $1 : (unLoc $3)) }

--------------------------------------------------------------------------------

enumTypeDefinition :: { EnumTypeDefinition }
enumTypeDefinition
  : description 'enum' name directives '{' enumValuesDefinition '}'
      { EnumTypeDefinition (fromMaybe (locate $2) (fmap locate $1) <> locate $7) (fmap unLoc $1) (unLoc $3) (fmap unLoc $4) $6 }

enumValuesDefinition :: { [EnumValueDefinition] }
enumValuesDefinition
  : enumValueDefinition { [$1] }
  | enumValueDefinition enumValuesDefinition { $1 : $2 }

enumValueDefinition :: { EnumValueDefinition }
enumValueDefinition
  : description name directives { EnumValueDefinition (fromMaybe (locate $2) (fmap locate $1) <> maybeLoc $2 $3) (fmap unLoc $1) (unLoc $2) (fmap unLoc $3) } 

--------------------------------------------------------------------------------

inputObjectTypeDefinition :: { InputObjectTypeDefinition }
inputObjectTypeDefinition
  : description 'input' name directives inputFieldsDefinition
      { InputObjectTypeDefinition (fromMaybe (locate $2) (fmap locate $1) <> maybeLoc (maybeLoc $3 $4) $5) (fmap unLoc $1) (unLoc $3) (fmap unLoc $4) $5 }

--------------------------------------------------------------------------------

directiveDefinition :: { DirectiveDefinition }
directivedefinition
  : description 'directive' '@' dir argumentsDefinition optRepeatable 'on' directiveLocations
      { DirectiveDefinition (fromMaybe (locate $2) (fmap locate $1) <> locate $8) (fmap unLoc $1) (Name $ unLoc $4) $5 (unLoc $8) }
  | description 'directive' '@' dir argumentsDefinition optRepeatable 'on' '|' directiveLocations
      { DirectiveDefinition (fromMaybe (locate $2) (fmap locate $1) <> locate $9) (fmap unLoc $1) (Name $ unLoc $4) $5 (unLoc $9) }

directiveLocations :: { Loc [DirectiveLocation] }
directivelocations
  : directiveLocation { Loc (locate $1) [$1] }
  | directiveLocation '|' directiveLocations { Loc (locate $1 <> locate $3) ($1 : unLoc $3) }

directiveLocation :: { DirectiveLocation }
directivelocation
: executableDirectiveLocation { ExecDirLoc (locate $1) (unLoc $1) }
| typeSystemDirectiveLocation { TypeSysDirLoc (locate $1) (unLoc $1) }

executableDirectiveLocation :: { Loc ExecutableDirectiveLocation }
executableDirectiveLocation
  : 'QUERY' { Loc (locate $1) EDLQUERY }
  | 'MUTATION' { Loc (locate $1) EDLMUTATION }
  | 'SUBSCRIPTION' { Loc (locate $1) EDLSUBSCRIPTION }
  | 'FIELD' { Loc (locate $1) EDLFIELD }
  | 'FRAGMENT_DEFINITION' { Loc (locate $1) EDLFRAGMENT_DEFINITION }
  | 'FRAGMENT_SPREAD' { Loc (locate $1) EDLFRAGMENT_SPREAD }
  | 'INLINE_FRAGMENT' { Loc (locate $1) EDLINLINE_FRAGMENT }
  | 'VARIABLE_DEFINITION' { Loc (locate $1) EDLVARIABLE_DEFINITION }

typeSystemDirectiveLocation :: { Loc TypeSystemDirectiveLocation }
typeSystemDirectiveLocation
: 'SCHEMA' { Loc (locate $1) TSDLSCHEMA }
| 'SCALAR' { Loc (locate $1) TSDLSCALAR }
| 'OBJECT' { Loc (locate $1) TSDLOBJECT }
| 'FIELD_DEFINITION' { Loc (locate $1) TSDLFIELD_DEFINITION }
| 'ARGUMENT_DEFINITION' { Loc (locate $1) TSDLARGUMENT_DEFINITION }
| 'INTERFACE' { Loc (locate $1) TSDLINTERFACE }
| 'UNION' { Loc (locate $1) TSDLUNION }
| 'ENUM' { Loc (locate $1) TSDLENUM }
| 'ENUM_VALUE' { Loc (locate $1) TSDLENUM_VALUE }
| 'INPUT_OBJECT' { Loc (locate $1) TSDLINPUT_OBJECT }
| 'INPUT_FIELD_DEFINITION' { Loc (locate $1) TSDLINPUT_FIELD_DEFINITION }

--------------------------------------------------------------------------------

inputFieldsDefinition :: { Maybe InputFieldsDefinition }
inputFieldsDefinition
  : '{' inputValuesDefinition '}' { Just (InputFieldsDefinition (locate $1 <> locate $3) $2) }
  | %prec LOW { Nothing }

argumentsDefinition :: { Maybe ArgumentsDefinition }
argumentsDefinition
  : '(' inputValuesDefinition ')' { Just (ArgumentsDefinition $2) }
  | { Nothing }

inputValuesDefinition :: { NE.NonEmpty InputValueDefinition }
inputValuesDefinition
  : inputValueDefinition { $1 NE.:| [] }
  | inputValueDefinition inputValuesDefinition { $1 NE.<| $2 }

inputValueDefinition :: { InputValueDefinition }
inputValueDefinition
  : description name ':' type optValue directives
      { InputValueDefinition (maybeLoc $2 $1 <> maybeLoc (maybeLoc $4 $5) $6) (fmap unLoc $1) (unLoc $2) $4 $5 (fmap unLoc $6) }

--------------------------------------------------------------------------------

executableDefinition :: { ExecutableDefinition }
executabledefinition
  : operationDefinition { ExecutableDefinitionOperation $1}
  | fragmentDefinition { ExecutableDefinitionFragment $1}

operationDefinition :: { OperationDefinition }
operationDefinition
 : operationType directives selectionSet
     { OperationDefinition (locate $1 <> locate $3 )(unLoc $1) Nothing mempty (fmap unLoc $2) (unLoc $3) }
 | operationType name directives selectionSet
     { OperationDefinition (locate $1 <> locate $4) (unLoc $1) (Just (unLoc $2)) mempty (fmap unLoc $3) (unLoc $4) }
 | operationType variableDefinitions directives selectionSet
     { OperationDefinition (locate $1 <> locate $4) (unLoc $1) Nothing (Just $2) (fmap unLoc $3) (unLoc $4) }
 | operationType name variableDefinitions directives selectionSet
     { OperationDefinition (locate $1 <> locate $5) (unLoc $1) (Just (unLoc $2)) (Just $3) (fmap unLoc $4) (unLoc $5) }
 | selectionSet
     { OperationDefinition (locate $1) OperationTypeQuery Nothing mempty Nothing (unLoc $1) }

--------------------------------------------------------------------------------

fragmentDefinition :: { FragmentDefinition }
fragmentDefinition
  : 'fragment' name typeCondition directives selectionSet
      { FragmentDefinition (locate $1 <> locate $5) (unLoc $2) $3 (fmap unLoc $4) (unLoc $5) }

fragmentSpread :: { FragmentSpread }
fragmentSpread
  : '...' name directives { FragmentSpread (locate $1 <> maybeLoc $2 $3) (unLoc $2) (fmap unLoc $3) }

inlineFragment :: { InlineFragment }
inlineFragment
  : '...' typeCondition directives selectionSet { InlineFragment (locate $1 <> locate $4) (Just $2) (fmap unLoc $3) (unLoc $4) }
  | '...' directives selectionSet { InlineFragment (locate $1 <> locate $3) Nothing (fmap unLoc $2) (unLoc $3) }

--------------------------------------------------------------------------------

field :: { Field }
field
  : aliasAndName directives { Field (locate $1 <> maybeLoc $1 $2) (fst (unLoc $1)) (snd (unLoc $1)) Nothing (fmap unLoc $2) mempty }
  | aliasAndName arguments directives { Field (locate $1 <> maybeLoc $2 $3) (fst (unLoc $1)) (snd (unLoc $1)) (Just $2) (fmap unLoc $3) mempty }
  | aliasAndName arguments directives selectionSet { Field (locate $1 <> locate $4) (fst (unLoc $1)) (snd (unLoc $1)) (Just $2) (fmap unLoc $3) (Just (unLoc $4)) }
  | aliasAndName directives selectionSet { Field (locate $1 <> locate $3) (fst (unLoc $1)) (snd (unLoc $1)) Nothing (fmap unLoc $2) (Just (unLoc $3)) }

--------------------------------------------------------------------------------

selectionSet :: { Loc SelectionSet }
selectionSet
  : '{' selections '}' { Loc (locate $1 <> locate $3) (SelectionSet (NE.fromList $2)) }

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

optValueConst :: { Maybe Value }
optValueConst
  : valueConst { Just $1 }
  | %prec LOW { Nothing }

values :: { [Value] }
values
  : value { [$1] }
  | value values { $1 : $2 }

value :: { Value }
value
  : valueConst { $1 }
  | variable { VVar (locate $1) (unLoc $1) }

valueConst :: { Value }
valueConst
  : 'null' { VNull (locate $1) }
  | stringValue { VString (locate $1) (unLoc $1) }
  | float { VFloat (locate $1) (unLoc $1) }
  | int { VInt (locate $1) (unLoc $1) }
  | bool { VBoolean (locate $1) (unLoc $1) }
  | vlist { $1 }
  | vobject { $1 }

variable :: { Loc Text }
variable
  : '$' ident { Loc (locate $1 <> locate $2) (unLoc $2) }

stringValue :: { Loc Text }
stringValue
  : '"' '"' { Loc (locate $1 <> locate $2) "" }
  | '"' string '"' { Loc (locate $1 <> locate $3) (unLoc $2) }
  | '"""' blockString '"""' { Loc (locate $1 <> locate $3) (unLoc $2) }
      
vlist :: { Value }
vlist
  : '[' ']' { VList (locate $1 <> locate $2) [] }
  | '[' values ']' { VList (locate $1 <> locate $3) $2 }

vobject :: { Value } 
vobject
  : '{' '}' { VObject (locate $1 <> locate $2) mempty }
  | '{' object '}' { VObject (locate $1 <> locate $3) $2 }

object :: { HashMap Name Value }
object
  : objectField { uncurry Map.singleton $1 }
  | objectField object { uncurry Map.insert $1 $2 }

objectField :: { (Name, Value) }
objectField
  : name ':' value { (unLoc $1, $3) }

--------------------------------------------------------------------------------

operationType :: { Loc OperationType }
operationtype
  : 'query' { Loc (locate $1) OperationTypeQuery }
  | 'mutation' { Loc (locate $1) OperationTypeMutation }
  | 'subscription' { Loc (locate $1) OperationTypeSubscription }

aliasAndName :: { Loc (Maybe Name, Name) }
aliasAndName
  : name ':' name { Loc (locate $1 <> locate $3) (Just (unLoc $1), (unLoc $3)) }
  | name { Loc (locate $1) (Nothing, (unLoc $1)) }

name :: { Loc Name }
name
  : ident { Loc (locate $1) (Name (unLoc $1)) }

arguments :: { Arguments }
arguments
  : '(' arguments_ ')' { Arguments (locate $2) (unLoc $2) }

arguments_ :: { Loc (HashMap Name Value) }
arguments_
  : argument { Loc (locate $1) (uncurry Map.singleton (unLoc $1)) }
  | arguments_ argument { Loc (locate $1 <> locate $2) (uncurry Map.insert (unLoc $2) (unLoc $1)) }

argument :: { Loc (Name, Value) }
argument
  : name ':' value { Loc (locate $1 <> locate $3) (unLoc $1, $3) }
  | '$' name ':' value { Loc (locate $1 <> locate $4) (unLoc $2, $4) }

variableDefinitions :: { VariablesDefinition }
variabledefinitions
  : '(' variableDefinitions_ ')' { VariablesDefinition $2 }

variableDefinitions_ :: { NE.NonEmpty VariableDefinition }
variabledefinitions_
  : variableDefinition { pure $1 }
  | variableDefinition variableDefinitions_ { $1 NE.<| $2 }

variableDefinition :: { VariableDefinition }
variabledefinition
  : variable ':' type optValueConst directives { VariableDefinition (locate $1 <> maybeLoc (maybeLoc $3 $4) $5) (Name $ unLoc $1) $3 $4 (fmap unLoc $5) }

typeCondition :: { TypeCondition }
typeCondition
  : 'on' name { TypeCondition (unLoc $2) }

type :: { Type }
type
  : name { NamedType (locate $1) (unLoc $1) }
  | type '!' { NonNullType (locate $1 <> locate $2) $1 }
  | '[' type ']' { ListType (locate $1 <> locate $3) $2 }

directives :: { Maybe (Loc Directives) }
directives
  : directives_ { Just (Loc (locate $1) (Directives (unLoc $1))) }
  | { Nothing }

directives_ :: { Loc (NE.NonEmpty Directive) }
directives_
  : directive { Loc (locate $1) ($1 NE.:| []) }
  | directive directives_ { Loc (locate $1 <> locate $2) ($1 NE.<| unLoc $2) }

directive :: { Directive }
directive
  : '@' dir { Directive (locate $1 <> locate $2) (Name $ unLoc $2) Nothing }
  | '@' dir arguments { Directive (locate $1 <> locate $3) (Name $ unLoc $2) (Just $3) }

description :: { Maybe (Loc Description) }
description
  : stringValue { Just $ Loc (locate $1) (Description (unLoc $1)) }
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
  src <- gets parseSource
  -- TODO: fix source position capture here. I think we need the prior span.
  parseError $ UnexpectedToken tok src
}
