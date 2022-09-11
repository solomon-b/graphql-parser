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
import Data.HashMap.Strict qualified as M
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
    [genName >>= \name -> pure $ NamedType dummySpan name]
    [ genType >>= \ty -> pure $ ListType dummySpan ty,
      genType >>= \ty -> pure $ NonNullType dummySpan ty
    ]

genDescription :: Gen Description
genDescription = Description <$> Gen.choice [genText, genBlockText]

genValue :: Gen Value
genValue =
  Gen.recursive
    Gen.choice
    [ genName >>= \name -> pure $ VVar dummySpan (unName name),
      pure $ VNull dummySpan,
      VInt dummySpan <$> Gen.integral (Range.linear 1 11),
      VFloat dummySpan . fromFloatDigits <$> Gen.realFloat (Range.constant (-10 :: Double) 10),
      VString dummySpan <$> Gen.text (Range.linear 0 11) alphaNum_,
      VBoolean dummySpan <$> Gen.bool,
      VEnum dummySpan . EnumValue <$> Gen.text (Range.linear 0 11) alphaNum_
    ]
    [ VList dummySpan <$> mkList genValue,
      VObject dummySpan . Map.fromList <$> mkList genArgument
    ]

-------------------------------------------------------------------------------
-- Values

genValueWith :: Gen Value
genValueWith = Gen.recursive Gen.choice nonRecursive recursive
  where
    recursive =
      [ genListValue genValueWith >>= \vals -> pure $ VList dummySpan vals,
        genObjectValue genValueWith >>= \vals -> pure $ VObject dummySpan vals
      ]
    -- TODO: use maxbound of int32/double or something?
    nonRecursive =
      [ pure $ VNull dummySpan,
        VInt dummySpan . fromIntegral <$> Gen.int32 (Range.linear 1 99999),
        VEnum dummySpan <$> genEnumValue,
        VFloat dummySpan . fromFloatDigits <$> Gen.double (Range.linearFrac 1.1 999999.99999),
        VString dummySpan <$> Gen.choice [genText, genBlockText],
        VBoolean dummySpan <$> Gen.bool
      ]
        <> [VVar dummySpan <$> genText]

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
    [ ExecutableDefinitionOperation . OperationDefinitionTyped <$> genOperationDefinition,
      ExecutableDefinitionOperation . OperationDefinitionUnTyped dummySpan <$> genSelectionSet,
      ExecutableDefinitionFragment <$> genFragmentDefinition
    ]

genOperationDefinition :: Gen TypedOperationDefinition
genOperationDefinition = do
  _odType <- genOperationType
  _odName <- Gen.maybe genName
  _odVariables <- Gen.maybe genVariablesDefinition
  _odDirectives <- Gen.maybe genDirectives
  _odSelectionSet <- genSelectionSet
  let _odSpan = dummySpan
  pure $ TypedOperationDefinition {..}

genVariablesDefinition :: Gen VariablesDefinition
genVariablesDefinition =
  fmap VariablesDefinition (mkListNonEmpty genVariableDefinition)

genVariableDefinition :: Gen VariableDefinition
genVariableDefinition = do
  _vdName <- genName
  _vdType <- genType
  _vdDefaultValue <- Gen.maybe genValue
  _vdDirectives <- Gen.maybe genDirectives
  let _vdSpan = dummySpan
  pure $ VariableDefinition {..}

genFragmentDefinition :: Gen FragmentDefinition
genFragmentDefinition = do
  _frdName <- genName
  _frdTypeCondition <- genTypeCondition
  _frdDirectives <- Gen.maybe genDirectives
  _frdSelectionSet <- genSelectionSet
  let _frdSpan = dummySpan
  pure $ FragmentDefinition {..}

genTypeSystemDefinition :: Gen TypeSystemDefinition
genTypeSystemDefinition =
  Gen.choice
    [ TypeSystemDefinitionSchema <$> genSchemaDefinition,
      TypeSystemDefinitionType <$> genTypeDefinition,
      TypeSystemDefinitionDirective <$> genDirectiveDefinition
    ]

genSchemaDefinition :: Gen SchemaDefinition
genSchemaDefinition = do
  _sdDescription <- Gen.maybe genDescription
  _sdDirectives <- Gen.maybe genDirectives
  _sdRootOperationTypeDefinitions <- fmap RootOperationTypesDefinition (mkListNonEmpty genRootOperationTypeDefinition)
  let _sdSpan = dummySpan
  pure $ SchemaDefinition {..}

genRootOperationTypeDefinition :: Gen RootOperationTypeDefinition
genRootOperationTypeDefinition = do
  _rotdOperationType <- genOperationType
  _rotdNamedType <- genName
  let _rotdSpan = dummySpan
  pure $ RootOperationTypeDefinition {..}

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
genScalarTypeDefinition = do
  _stDescription <- Gen.maybe genDescription
  _stName <- genName
  _stDirectives <- Gen.maybe genDirectives
  let _stSpan = dummySpan
  pure $ ScalarTypeDefinition {..}

genObjectTypeDefinition :: Gen ObjectTypeDefinition
genObjectTypeDefinition = do
  _otdDescription <- Gen.maybe genDescription
  _otdName <- genName
  _otdImplementsInterfaces <- Gen.maybe genImplementsInterfaces
  _otdDirectives <- Gen.maybe genDirectives
  _otdFieldsDefinition <- Gen.maybe genFieldDefinitions
  let _otdSpan = dummySpan
  pure $ ObjectTypeDefinition {..}

genInterfaceTypeDefinition :: Gen InterfaceTypeDefinition
genInterfaceTypeDefinition = do
  _itDescription <- Gen.maybe genDescription
  _itName <- genName
  _itInterfaces <- Gen.maybe genImplementsInterfaces
  _itDirectives <- Gen.maybe genDirectives
  _itFields <- Gen.maybe genFieldDefinitions
  let _itSpan = dummySpan
  pure $ InterfaceTypeDefinition {..}

genUnionTypeDefinition :: Gen UnionTypeDefinition
genUnionTypeDefinition =
  UnionTypeDefinition
    <$> pure dummySpan
    <*> Gen.maybe genDescription
    <*> genName
    <*> Gen.maybe genDirectives
    <*> mkList genName

genEnumTypeDefinition :: Gen EnumTypeDefinition
genEnumTypeDefinition = do
  _etdDescription <- Gen.maybe genDescription
  _etdName <- genName
  _etdDirectives <- Gen.maybe genDirectives
  _etdValueDefinitions <- mkList genEnumValueDefinition
  let _etdSpan = dummySpan
  pure $ EnumTypeDefinition {..}

genInputObjectTypeDefinition :: Gen InputObjectTypeDefinition
genInputObjectTypeDefinition = do
  _iotDescription <- Gen.maybe genDescription
  _iotName <- genName
  _iotDirectives <- Gen.maybe genDirectives
  _iotValueDefinitions <- Gen.maybe genInputFieldsDefinition
  let _iotSpan = dummySpan
  pure $ InputObjectTypeDefinition {..}

genInputValueDefinition :: Gen InputValueDefinition
genInputValueDefinition = do
  _ivdDescription <- Gen.maybe genDescription
  _ivdName <- genName
  _ivdType <- genType
  _ivdDefaultValue <- Gen.maybe genValue
  _ivdDirectives <- Gen.maybe genDirectives
  let _ivdSpan = dummySpan
  pure $ InputValueDefinition {..}

genEnumValueDefinition :: Gen EnumValueDefinition
genEnumValueDefinition = do
  _evdDescription <- Gen.maybe genDescription
  _evdName <- genName
  _evdDirectives <- Gen.maybe genDirectives
  let _evdSpan = dummySpan
  pure $ EnumValueDefinition {..}

genImplementsInterfaces :: Gen ImplementsInterfaces
genImplementsInterfaces =
  mkListNonEmpty genName >>= \names -> pure $ ImplementsInterfaces dummySpan names

genFieldDefinition :: Gen FieldDefinition
genFieldDefinition = do
  _fldDescription <- Gen.maybe genDescription
  _fldName <- genName
  _fldArgumentsDefinition <- Gen.maybe genArgumentsDefinition
  _fldType <- genType
  _fldDirectives <- Gen.maybe genDirectives
  let _fldSpan = dummySpan
  pure $ FieldDefinition {..}

genFieldDefinitions :: Gen FieldsDefinition
genFieldDefinitions =
  mkListNonEmpty genFieldDefinition >>= \defs ->
    pure $ FieldsDefinition dummySpan defs

genDirectiveDefinition :: Gen DirectiveDefinition
genDirectiveDefinition = do
  _ddDescription <- Gen.maybe genDescription
  _ddName <- genName
  _ddArguments <- Gen.maybe genArgumentsDefinition
  _ddLocations <- Gen.list (Range.linear 1 10) genDirectiveLocation
  let _ddSpan = dummySpan
  pure $ DirectiveDefinition {..}

genInputFieldsDefinition :: Gen InputFieldsDefinition
genInputFieldsDefinition =
  mkListNonEmpty genInputValueDefinition >>= \defs ->
    pure $ InputFieldsDefinition dummySpan defs

genArgumentsDefinition :: Gen ArgumentsDefinition
genArgumentsDefinition = ArgumentsDefinition <$> mkListNonEmpty genInputValueDefinition

genDirectiveLocation :: Gen DirectiveLocation
genDirectiveLocation =
  Gen.choice
    [ genExecutableDirectiveLocation >>= \loc -> pure $ ExecDirLoc dummySpan loc,
      genTypeSystemDirectiveLocation >>= \loc -> pure $ TypeSysDirLoc dummySpan loc
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
genFragmentSpread = do
  _fsName <- genName
  _fsDirectives <- Gen.maybe genDirectives
  let _fsSpan = dummySpan
  pure $ FragmentSpread {..}

genInlineFragment :: Gen InlineFragment
genInlineFragment = do
  _ifTypeCondition <- Gen.maybe genTypeCondition
  _ifDirectives <- Gen.maybe genDirectives
  _ifSelectionSet <- genSelectionSet
  let _ifSpan = dummySpan
  pure $ InlineFragment {..}

genField :: Gen Field
genField = do
  _fAlias <- Gen.maybe genName
  _fName <- genName
  _fArguments <- Gen.maybe genArguments
  _fDirectives <- Gen.maybe genDirectives
  _fSelectionSet <- Gen.maybe genSelectionSet
  let _fSpan = dummySpan
  pure $ Field {..}

genDirective :: Gen Directive
genDirective = do
  _dName <- genName
  _dArguments <- Gen.maybe genArguments
  let _dSpan = dummySpan
  pure $ Directive {..}

genDirectives :: Gen Directives
genDirectives = Directives <$> mkListNonEmpty genDirective

genArguments :: Gen Arguments
genArguments = fmap (Arguments dummySpan) (M.fromList <$> mkList genArgument)

genArgument :: Gen (Name, Value)
genArgument = (,) <$> genName <*> genValue

-------------------------------------------------------------------------------
-- Helpers

mkList :: Gen a -> Gen [a]
mkList = Gen.list $ Range.linear 0 11

mkListNonEmpty :: Gen a -> Gen (NE.NonEmpty a)
mkListNonEmpty = Gen.nonEmpty $ Range.linear 1 11

-- | Placeholder span for generators
dummySpan :: Span
dummySpan = Span (AlexSourcePos 1 1) (AlexSourcePos 1 2)
