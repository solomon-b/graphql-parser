module GraphQLParser.Generator
  ( -- * Generator
    generate,

    -- * Document
    genDocument,

    -- * Identifiers
    genText,
    alpha_,
    alphaNum_,
    genGraphqlName,
    genName,
    genType,
    genDescription,
    genValueWith,
    genEnumValue,
    genListValue,
    genObjectValue,
    genBlockText,
    genMinIndentedText,
    genIndentation,

    -- * Definitions
    genExecutableDefinition,
    genOperationDefinition,
    genVariableDefinition,
    genFragmentDefinition,
    genTypeSystemDefinition,
    genSchemaDefinition,
    genRootOperationTypeDefinition,
    genOperationType,
    genTypeDefinition,
    genScalarTypeDefinition,
    genObjectTypeDefinition,
    genInterfaceTypeDefinition,
    genUnionTypeDefinition,
    genEnumTypeDefinition,
    genInputObjectTypeDefinition,
    genInputValueDefinition,
    genEnumValueDefinition,
    genFieldDefinition,
    genFieldDefinitions,
    genDirectiveDefinition,
    genArgumentsDefinition,
    genDirectiveLocation,
    genExecutableDirectiveLocation,
    genTypeSystemDirectiveLocation,

    -- * Structure
    genSelectionSet,
    genSelection,
    genFragmentSpread,
    genInlineFragment,
    genField,
    genDirective,
    genDirectives,
    genArgument,

    -- * Helpers
    mkList,
    mkListNonEmpty,
  )
where

-------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO)
import Data.HashMap.Strict as M
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import Data.Text qualified as T
import GraphQLParser
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Prelude

-------------------------------------------------------------------------------

generate :: MonadIO m => Gen a -> m a
generate = Gen.sample

-------------------------------------------------------------------------------
-- Document

genDocument :: Gen Document
genDocument =
  Document <$> Gen.list (Range.linear 0 3) (Gen.choice [DefinitionExecutable <$> genExecutableDefinition, DefinitionTypeSystem <$> genTypeSystemDefinition])

-------------------------------------------------------------------------------
-- Identifiers

genText :: Gen Text
genText = Gen.text (Range.linear 0 11) Gen.unicode

alpha_ :: Gen Char
alpha_ = Gen.choice [Gen.alpha, pure '_']

alphaNum_ :: Gen Char
alphaNum_ = Gen.choice [Gen.alphaNum, pure '_']

genGraphqlName :: Gen Text
genGraphqlName =
  Gen.text (Range.singleton 1) alpha_
    <> Gen.text (Range.linear 0 11) alphaNum_

genName :: Gen Name
genName = Name <$> genGraphqlName

genTypeCondition :: Gen TypeCondition
genTypeCondition = fmap TypeCondition genName

genType :: Gen GraphQLParser.Type
genType =
  Gen.recursive
    Gen.choice
    [NamedType <$> genName]
    [ListType <$> genType, NonNullType <$> genType]

genDescription :: Gen Description
genDescription = Description <$> Gen.choice [genText, genBlockText]

genValue :: Gen Value
genValue =
  Gen.recursive
    Gen.choice
    [ VVar . unName <$> genName,
      pure VNull,
      VInt <$> Gen.integral (Range.linear 1 11),
      VFloat . fromFloatDigits <$> Gen.realFloat (Range.constant (-10 :: Double) 10),
      VString <$> Gen.text (Range.linear 0 11) alphaNum_,
      VBoolean <$> Gen.bool,
      VEnum . EnumValue <$> Gen.text (Range.linear 0 11) alphaNum_
    ]
    [ VList <$> mkList genValue,
      VObject . Map.fromList <$> mkList genArgument
    ]

-------------------------------------------------------------------------------
-- Values

genValueWith :: Gen Value
genValueWith = Gen.recursive Gen.choice nonRecursive recursive
  where
    recursive =
      [ VList <$> genListValue genValueWith,
        VObject <$> genObjectValue genValueWith
      ]
    -- TODO: use maxbound of int32/double or something?
    nonRecursive =
      [ pure VNull,
        VInt . fromIntegral <$> Gen.int32 (Range.linear 1 99999),
        VEnum <$> genEnumValue,
        VFloat . fromFloatDigits <$> Gen.double (Range.linearFrac 1.1 999999.99999),
        VString <$> Gen.choice [genText, genBlockText],
        VBoolean <$> Gen.bool
      ]
        <> [VVar <$> genText]

genEnumValue :: Gen EnumValue
genEnumValue = EnumValue . unName <$> genName

genListValue :: Gen Value -> Gen [Value]
genListValue = mkList

genObjectValue :: Gen Value -> Gen (M.HashMap Name Value)
genObjectValue genVal = M.fromList <$> mkList genObjectField
  where
    genObjectField = (,) <$> genName <*> genVal

genBlockText :: Gen Text
genBlockText = T.unlines <$> Gen.list (Range.linear 0 20) line'
  where
    line' = do
      Gen.frequency
        [ (10, Gen.text (Range.linear 1 10) Gen.unicode),
          (10, return "\n"),
          (6, genIndentation),
          (5, genMinIndentedText 10),
          (4, return ""),
          (3, return " "),
          (6, return "\t"),
          (3, return "\""), -- "
          (3, return "\\") -- \
        ]

-- | Like `genText` but with random indentation in the start of the string according
-- to a minimum value.
genMinIndentedText :: Int -> Gen Text
genMinIndentedText min_ = do
  let minIndent = T.replicate min_ " "
  i <- genIndentation
  t <- genText
  return (minIndent <> i <> t)

genIndentation :: Gen Text
genIndentation = do
  Gen.text (Range.linear 0 100) (return ' ')

-------------------------------------------------------------------------------
-- Definitions

genExecutableDefinition :: Gen ExecutableDefinition
genExecutableDefinition =
  Gen.choice
    [ ExecutableDefinitionOperation <$> genOperationDefinition,
      ExecutableDefinitionFragment <$> genFragmentDefinition
    ]

genOperationDefinition :: Gen OperationDefinition
genOperationDefinition =
  OperationDefinition
    <$> genOperationType
    <*> Gen.maybe genName
    <*> Gen.maybe genVariablesDefinition
    <*> Gen.maybe genDirectives
    <*> genSelectionSet

genVariablesDefinition :: Gen VariablesDefinition
genVariablesDefinition =
  fmap VariablesDefinition (mkListNonEmpty genVariableDefinition)

genVariableDefinition :: Gen VariableDefinition
genVariableDefinition =
  VariableDefinition
    <$> genName
    <*> genType
    <*> Gen.maybe genValue
    <*> Gen.maybe genDirectives

genFragmentDefinition :: Gen FragmentDefinition
genFragmentDefinition =
  FragmentDefinition
    <$> genName
    <*> genTypeCondition
    <*> Gen.maybe genDirectives
    <*> genSelectionSet

genTypeSystemDefinition :: Gen TypeSystemDefinition
genTypeSystemDefinition =
  Gen.choice
    [ TypeSystemDefinitionSchema <$> genSchemaDefinition,
      TypeSystemDefinitionType <$> genTypeDefinition,
      TypeSystemDefinitionDirective <$> genDirectiveDefinition
    ]

genSchemaDefinition :: Gen SchemaDefinition
genSchemaDefinition =
  SchemaDefinition
    <$> Gen.maybe genDescription
    <*> Gen.maybe genDirectives
    <*> fmap RootOperationTypesDefinition (mkListNonEmpty genRootOperationTypeDefinition)

genRootOperationTypeDefinition :: Gen RootOperationTypeDefinition
genRootOperationTypeDefinition =
  RootOperationTypeDefinition
    <$> genOperationType
    <*> genName

genOperationType :: Gen OperationType
genOperationType =
  Gen.element
    [ OperationTypeQuery,
      OperationTypeMutation,
      OperationTypeSubscription
    ]

genTypeDefinition :: Gen TypeDefinition
genTypeDefinition =
  Gen.choice
    [ STDef <$> genScalarTypeDefinition,
      OTDef <$> genObjectTypeDefinition,
      ITDef <$> genInterfaceTypeDefinition,
      UTDef <$> genUnionTypeDefinition,
      ETDef <$> genEnumTypeDefinition,
      IOTDef <$> genInputObjectTypeDefinition
    ]

genScalarTypeDefinition :: Gen ScalarTypeDefinition
genScalarTypeDefinition =
  ScalarTypeDefinition
    <$> Gen.maybe genDescription
    <*> genName
    <*> Gen.maybe genDirectives

genObjectTypeDefinition :: Gen ObjectTypeDefinition
genObjectTypeDefinition =
  ObjectTypeDefinition
    <$> Gen.maybe genDescription
    <*> genName
    <*> Gen.maybe genImplementsInterfaces
    <*> Gen.maybe genDirectives
    <*> Gen.maybe genFieldDefinitions

genInterfaceTypeDefinition :: Gen InterfaceTypeDefinition
genInterfaceTypeDefinition =
  InterfaceTypeDefinition
    <$> Gen.maybe genDescription
    <*> genName
    <*> Gen.maybe genImplementsInterfaces
    <*> Gen.maybe genDirectives
    <*> Gen.maybe genFieldDefinitions

genUnionTypeDefinition :: Gen UnionTypeDefinition
genUnionTypeDefinition =
  UnionTypeDefinition
    <$> Gen.maybe genDescription
    <*> genName
    <*> Gen.maybe genDirectives
    <*> mkList genName

genEnumTypeDefinition :: Gen EnumTypeDefinition
genEnumTypeDefinition =
  EnumTypeDefinition
    <$> Gen.maybe genDescription
    <*> genName
    <*> Gen.maybe genDirectives
    <*> mkList genEnumValueDefinition

genInputObjectTypeDefinition :: Gen InputObjectTypeDefinition
genInputObjectTypeDefinition =
  InputObjectTypeDefinition
    <$> Gen.maybe genDescription
    <*> genName
    <*> Gen.maybe genDirectives
    <*> genInputFieldsDefinition

genInputValueDefinition :: Gen InputValueDefinition
genInputValueDefinition =
  InputValueDefinition
    <$> Gen.maybe genDescription
    <*> genName
    <*> genType
    <*> Gen.maybe genValue
    <*> Gen.maybe genDirectives

genEnumValueDefinition :: Gen EnumValueDefinition
genEnumValueDefinition =
  EnumValueDefinition
    <$> Gen.maybe genDescription
    <*> genName
    <*> Gen.maybe genDirectives

genImplementsInterfaces :: Gen ImplementsInterfaces
genImplementsInterfaces =
  ImplementsInterfaces <$> mkListNonEmpty genName

genFieldDefinition :: Gen FieldDefinition
genFieldDefinition =
  FieldDefinition
    <$> Gen.maybe genDescription
    <*> genName
    <*> genArgumentsDefinition
    <*> genType
    <*> Gen.maybe genDirectives

genFieldDefinitions :: Gen FieldsDefinition
genFieldDefinitions = FieldsDefinition <$> mkListNonEmpty genFieldDefinition

genDirectiveDefinition :: Gen DirectiveDefinition
genDirectiveDefinition =
  DirectiveDefinition
    <$> Gen.maybe genDescription
    <*> genName
    <*> genArgumentsDefinition
    <*> Gen.list (Range.linear 1 10) genDirectiveLocation

genInputFieldsDefinition :: Gen InputFieldsDefinition
genInputFieldsDefinition = InputFieldsDefinition <$> mkList genInputValueDefinition

genArgumentsDefinition :: Gen ArgumentsDefinition
genArgumentsDefinition = ArgumentsDefinition <$> Gen.list (Range.linear 1 10) genInputValueDefinition

genDirectiveLocation :: Gen DirectiveLocation
genDirectiveLocation =
  Gen.choice
    [ ExecDirLoc <$> genExecutableDirectiveLocation,
      TypeSysDirLoc <$> genTypeSystemDirectiveLocation
    ]

genExecutableDirectiveLocation :: Gen ExecutableDirectiveLocation
genExecutableDirectiveLocation =
  Gen.element
    [ EDLQUERY,
      EDLMUTATION,
      EDLSUBSCRIPTION,
      EDLFIELD,
      EDLFRAGMENT_DEFINITION,
      EDLFRAGMENT_SPREAD,
      EDLINLINE_FRAGMENT
    ]

genTypeSystemDirectiveLocation :: Gen TypeSystemDirectiveLocation
genTypeSystemDirectiveLocation =
  Gen.element
    [ TSDLSCHEMA,
      TSDLSCALAR,
      TSDLOBJECT,
      TSDLFIELD_DEFINITION,
      TSDLARGUMENT_DEFINITION,
      TSDLINTERFACE,
      TSDLUNION,
      TSDLENUM,
      TSDLENUM_VALUE,
      TSDLINPUT_OBJECT,
      TSDLINPUT_FIELD_DEFINITION
    ]

-------------------------------------------------------------------------------
-- Structure

genSelectionSet :: Gen SelectionSet
genSelectionSet = SelectionSet <$> mkListNonEmpty genSelection

genSelection :: Gen Selection
genSelection =
  Gen.recursive
    Gen.choice
    [ SelectionFragmentSpread <$> genFragmentSpread
    ]
    [ SelectionField <$> genField,
      SelectionInlineFragment <$> genInlineFragment
    ]

genFragmentSpread :: Gen FragmentSpread
genFragmentSpread =
  FragmentSpread
    <$> genName
    <*> Gen.maybe genDirectives

genInlineFragment :: Gen InlineFragment
genInlineFragment =
  InlineFragment
    <$> Gen.maybe genTypeCondition
    <*> Gen.maybe genDirectives
    <*> genSelectionSet

genField :: Gen Field
genField =
  Field
    <$> Gen.maybe genName
    <*> genName
    <*> genArguments
    <*> Gen.maybe genDirectives
    <*> Gen.maybe genSelectionSet

genDirective :: Gen Directive
genDirective =
  Directive
    <$> genName
    <*> Gen.maybe genArguments

genDirectives :: Gen Directives
genDirectives = Directives <$> mkListNonEmpty genDirective

genArguments :: Gen Arguments
genArguments = fmap Arguments (M.fromList <$> mkList genArgument)

genArgument :: Gen (Name, Value)
genArgument = (,) <$> genName <*> genValue

-------------------------------------------------------------------------------
-- Helpers

mkList :: Gen a -> Gen [a]
mkList = Gen.list $ Range.linear 0 11

mkListNonEmpty :: Gen a -> Gen (NE.NonEmpty a)
mkListNonEmpty = Gen.nonEmpty $ Range.linear 1 11
