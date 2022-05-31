{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}

-- | GraphQL IR
module GraphQLParser.Syntax where

import Control.DeepSeq (NFData)
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Char qualified as C
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
import Data.Hashable (Hashable)
import Data.List.NonEmpty qualified as NE
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)
import Prettyprinter (Doc, Pretty (..), defaultLayoutOptions, dquote, encloseSep, flatAlt, group, layoutPretty, line, punctuate, sep, space, tupled, (<+>))
import Prettyprinter.Render.Text (renderStrict)

type a \/ b = Either a b

--------------------------------------------------------------------------------

-- | A GraphQL 'Document' describes a complete file or request string
-- operated on by a GraphQL service or client. A 'Document' contains
-- multiple 'Definition's, either executable or representative of a
-- GraphQL type system.
newtype Document definition = Document [definition]
  deriving stock (Lift)
  deriving newtype (Eq, Ord, Show, Read, NFData)

instance Pretty definition => Pretty (Document definition) where
  pretty (Document defs) = foldMap ((<> line) . pretty) defs

-- | A GraphQL Document describes a complete file or request string
-- operated on by a GraphQL service or client. A document contains
-- multiple definitions, either executable or representative of a
-- GraphQL type system.
type GraphQLDocument = Document GraphQLDefinition

data GraphQLDefinition = ExecDef ExecutableDefinition | TypeSysDef TypeSystemDefinition
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty GraphQLDefinition where
  pretty = \case
    ExecDef ed -> pretty ed
    TypeSysDef ts -> pretty ts

-- | A 'Document' containing strictly '[ExecutableDefinition]' where at
-- least one value is a 'OperationDefinition' is considered an
-- Executable Document.
--
-- 'Document's are only executable by a GraphQL service if they are
-- 'ExecutableDocument'.
type ExecutableDocument = Document ExecutableDefinition

type TypeSystemDocument = Document TypeSystemDefinitionOrExtension

--------------------------------------------------------------------------------
-- Type System Definitions

-- | A 'Document' which contains 'TypeSystemDefinitionOrExtension' must
-- not be executed; GraphQL execution services which receive a
-- 'Document' containing these should return a descriptive error.
data TypeSystemDefinitionOrExtension
  = TyDefinition TypeSystemDefinition
  | TyExtension TypeSystemExtension
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

data TypeSystemDefinition
  = SchemaDef SchemaDefinition
  | TypeDef TypeDefinition
  | DirDef DirectiveDefinition
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty TypeSystemDefinition where
  pretty = \case
    SchemaDef sd -> pretty sd
    TypeDef ty -> pretty ty
    DirDef dd -> pretty dd

--------------------------------------------------------------------------------
-- Schema Definitions

-- | A GraphQL service’s collective type system capabilities are
-- referred to as that service’s "schema". A schema is defined in
-- terms of the types and directives it supports as well as the root
-- operation types for each kind of operation: query, mutation, and
-- subscription; this determines the place in the type system where
-- those operations begin.
data SchemaDefinition = SchemaDefinition
  { sdDescription :: Maybe Description,
    sdDirectives :: Directives,
    sdRootOperations :: RootOperationTypesDefinition
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty SchemaDefinition where
  pretty SchemaDefinition {..} =
    sep
      [ pretty sdDescription,
        "schema",
        pretty sdDirectives,
        pretty sdRootOperations
      ]

newtype RootOperationTypesDefinition = RootOperationTypesDefinition
  {unRootOperationTypesDefinition :: NE.NonEmpty RootOperationTypeDefinition}
  deriving stock (Lift, Generic)
  deriving newtype (Eq, Ord, Show, Read)
  deriving anyclass (NFData)

instance Pretty RootOperationTypesDefinition where
  pretty RootOperationTypesDefinition {..} =
    encloseSep "{" "}" line (foldr (\x xs -> pretty x : xs) mempty unRootOperationTypesDefinition)

-- | A 'SchemaDefiniton' defines the initial root operation type for
-- each kind of operation it supports: @Query@, @Mutation@, and
-- @Subscription@; this determines the place in the type system where
-- those operations begin.
data RootOperationTypeDefinition = RootOperationTypeDefinition
  { roType :: OperationType,
    roTypeName :: Name
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty RootOperationTypeDefinition where
  pretty RootOperationTypeDefinition {..} =
    sep [pretty roType <> ":", pretty roTypeName]

--------------------------------------------------------------------------------
-- Type Definitions

-- | The fundamental unit of any GraphQL Schema is the type.
data TypeDefinition
  = STDef ScalarTypeDefinition
  | OTDef ObjectTypeDefinition
  | ITDef InterfaceTypeDefinition
  | UTDef UnionTypeDefinition
  | ETDef EnumTypeDefinition
  | IOTDef InputObjectTypeDefinition
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty TypeDefinition where
  pretty = \case
    STDef std -> pretty std
    OTDef otd -> pretty otd
    ITDef itd -> pretty itd
    UTDef utd -> pretty utd
    ETDef etd -> pretty etd
    IOTDef iotd -> pretty iotd

data ScalarTypeDefinition = ScalarTypeDefinition
  { scalarDescription :: Maybe Description,
    scalarName :: Name,
    scalarDirectives :: Directives
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty ScalarTypeDefinition where
  pretty ScalarTypeDefinition {..} =
    sep
      [ pretty scalarDescription,
        "scalar",
        pretty scalarName,
        pretty scalarDirectives
      ]

-- | GraphQL operations are hierarchical and composed, describing a
-- tree of information. While Scalar types describe the leaf values of
-- these hierarchical operations, Objects describe the intermediate
-- levels.
data ObjectTypeDefinition = ObjectTypeDefinition
  { objectDescription :: Maybe Description,
    objectName :: Name,
    objectInterfaces :: Maybe ImplementsInterfaces,
    objectDirectives :: Directives,
    objectFields :: Maybe FieldsDefinition
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty ObjectTypeDefinition where
  pretty ObjectTypeDefinition {..} =
    sep
      [ pretty objectDescription,
        "type",
        pretty objectName,
        pretty objectInterfaces,
        pretty objectDirectives,
        pretty objectFields
      ]

newtype FieldsDefinition = FieldsDefinition {unFieldsDefinition :: NE.NonEmpty FieldDefinition}
  deriving stock (Lift, Generic)
  deriving newtype (Eq, Ord, Show, Read)
  deriving anyclass (NFData)

instance Pretty FieldsDefinition where
  pretty FieldsDefinition {..} =
    encloseSep "{" "}" line (foldr (\x xs -> pretty x : xs) [] unFieldsDefinition)

data FieldDefinition = FieldDefinition
  { fieldDefDescription :: Maybe Description,
    fieldDefName :: Name,
    fieldDefArgumentsDef :: ArgumentsDefinition,
    fieldDefType :: Type,
    fieldDefDirectives :: Directives
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty FieldDefinition where
  pretty FieldDefinition {..} =
    sep
      [ pretty fieldDefDescription,
        pretty fieldDefName,
        pretty fieldDefArgumentsDef,
        ":",
        pretty fieldDefType,
        pretty fieldDefDirectives
      ]

-- | GraphQL interfaces represent a list of named fields and their
-- arguments. GraphQL objects and interfaces can then implement these
-- interfaces which requires that the implementing type will define
-- all fields defined by those interfaces.
data InterfaceTypeDefinition = InterfaceTypeDefinition
  { interfaceDescription :: Maybe Description,
    interfaceName :: Name,
    interfaceInterfaces :: Maybe ImplementsInterfaces,
    interfaceDirectives :: Directives,
    interfaceFields :: Maybe FieldsDefinition
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty InterfaceTypeDefinition where
  pretty InterfaceTypeDefinition {..} =
    sep
      [ pretty interfaceDescription,
        "interface",
        pretty interfaceName,
        pretty interfaceInterfaces,
        pretty interfaceDirectives,
        pretty interfaceFields
      ]

newtype ImplementsInterfaces = ImplementsInterfaces {unInterfaces :: NE.NonEmpty Name}
  deriving stock (Lift, Generic)
  deriving newtype (Eq, Ord, Show, Read)
  deriving anyclass (NFData)

instance Pretty ImplementsInterfaces where
  pretty ImplementsInterfaces {..} =
    "implements" <+> foldMap (\n -> "&" <+> pretty n) unInterfaces

-- | GraphQL Unions represent an object that could be one of a list of
-- GraphQL Object types, but provides for no guaranteed fields between
-- those types. They also differ from interfaces in that Object types
-- declare what interfaces they implement, but are not aware of what
-- unions contain them.
data UnionTypeDefinition = UnionTypeDefinition
  { unionDescription :: Maybe Description,
    unionName :: Name,
    unionDirectives :: Directives,
    unionMemberTypes :: [Name]
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty UnionTypeDefinition where
  pretty UnionTypeDefinition {..} =
    sep
      [ pretty unionDescription,
        "union",
        pretty unionName,
        pretty unionDirectives,
        "=",
        foldMap (\n -> "|" <+> pretty n <> line) unionMemberTypes
      ]

-- | GraphQL Enum types, like Scalar types, also represent leaf values
-- in a GraphQL type system. However Enum types describe the set of
-- possible values.
data EnumTypeDefinition = EnumTypeDefinition
  { enumDescription :: Maybe Description,
    enumName :: Name,
    enumDirectives :: Directives,
    enumValues :: [EnumValueDefinition]
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty EnumTypeDefinition where
  pretty EnumTypeDefinition {..} =
    sep
      [ pretty enumDescription,
        "enum",
        pretty enumName,
        pretty enumDirectives,
        encloseSep "{" "}" line (fmap pretty enumValues)
      ]

data EnumValueDefinition = EnumValueDefinition
  { evDescription :: Maybe Description,
    evName :: Name,
    evDirectives :: Directives
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty EnumValueDefinition where
  pretty EnumValueDefinition {..} =
    sep
      [ pretty evDescription,
        pretty evName,
        pretty evDirectives
      ]

-- | A GraphQL Input Object defines a set of input fields; the input
-- fields are either scalars, enums, or other input objects. This
-- allows arguments to accept arbitrarily complex structs.
data InputObjectTypeDefinition = InputObjectTypeDefinition
  { inputDescription :: Maybe Description,
    inputName :: Name,
    inputDirectives :: Directives,
    inputValues :: InputFieldsDefinition
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty InputObjectTypeDefinition where
  pretty InputObjectTypeDefinition {..} =
    sep
      [ pretty inputDescription,
        "input",
        pretty inputName,
        pretty inputDirectives,
        pretty inputValues
      ]

newtype InputFieldsDefinition = InputFieldsDefinition {unInputFieldsDefinition :: [InputValueDefinition]}
  deriving stock (Lift, Generic)
  deriving newtype (Eq, Ord, Show, Read, Semigroup, Monoid)
  deriving anyclass (NFData)

instance Pretty InputFieldsDefinition where
  pretty InputFieldsDefinition {..} =
    encloseSep "{" "}" " " (fmap pretty unInputFieldsDefinition)

newtype ArgumentsDefinition = ArgumentsDefinition {unArgumentsDefinition :: [InputValueDefinition]}
  deriving stock (Lift, Generic)
  deriving newtype (Eq, Ord, Show, Read, Semigroup, Monoid)
  deriving anyclass (NFData)

instance Pretty ArgumentsDefinition where
  pretty ArgumentsDefinition {..} =
    case unArgumentsDefinition of
      [] -> ""
      xs -> encloseSep "(" ")" " " (fmap pretty xs)

data InputValueDefinition = InputValueDefinition
  { inputValueDescription :: Maybe Description,
    inputValueName :: Name,
    inputValueType :: Type,
    inputValueDefault :: Maybe Value,
    inputValueDirectives :: Directives
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty InputValueDefinition where
  pretty InputValueDefinition {..} =
    sep
      [ pretty inputValueDescription,
        pretty inputValueName <> ":",
        pretty inputValueType,
        pretty inputValueDefault,
        pretty inputValueDirectives
      ]

--------------------------------------------------------------------------------
-- Directive Definitions

data DirectiveDefinition = DirectiveDefinition
  { dirDefDescription :: Maybe Description,
    dirDefName :: Name,
    dirDefArguments :: ArgumentsDefinition,
    dirDefLocations :: [DirectiveLocation]
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty DirectiveDefinition where
  pretty DirectiveDefinition {..} =
    sep
      [ pretty dirDefDescription,
        "directive",
        "@" <> pretty dirDefName,
        pretty dirDefArguments,
        "on",
        foldMap (\n -> "|" <+> pretty n <> line) dirDefLocations
      ]

data DirectiveLocation = ExecDirLoc ExecutableDirectiveLocation | TypeSysDirLoc TypeSystemDirectiveLocation
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty DirectiveLocation where
  pretty = \case
    ExecDirLoc edl -> pretty edl
    TypeSysDirLoc tsdl -> pretty tsdl

data ExecutableDirectiveLocation
  = EDLQUERY
  | EDLMUTATION
  | EDLSUBSCRIPTION
  | EDLFIELD
  | EDLFRAGMENT_DEFINITION
  | EDLFRAGMENT_SPREAD
  | EDLINLINE_FRAGMENT
  | EDLVARIABLE_DEFINITION
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty ExecutableDirectiveLocation where
  pretty = \case
    EDLQUERY -> "QUERY"
    EDLMUTATION -> "MUTATION"
    EDLSUBSCRIPTION -> "SUBSCRIPTION"
    EDLFIELD -> "FIELD"
    EDLFRAGMENT_DEFINITION -> "FRAGMENT_DEFINITION"
    EDLFRAGMENT_SPREAD -> "FRAGMENT_SPREAD"
    EDLINLINE_FRAGMENT -> "INLINE_FRAGMENT"
    EDLVARIABLE_DEFINITION -> "VARIABLE_DEFINITION"

data TypeSystemDirectiveLocation
  = TSDLSCHEMA
  | TSDLSCALAR
  | TSDLOBJECT
  | TSDLFIELD_DEFINITION
  | TSDLARGUMENT_DEFINITION
  | TSDLINTERFACE
  | TSDLUNION
  | TSDLENUM
  | TSDLENUM_VALUE
  | TSDLINPUT_OBJECT
  | TSDLINPUT_FIELD_DEFINITION
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty TypeSystemDirectiveLocation where
  pretty = \case
    TSDLSCHEMA -> "SCHEMA"
    TSDLSCALAR -> "SCALAR"
    TSDLOBJECT -> "OBJECT"
    TSDLFIELD_DEFINITION -> "FIELD_DEFINITION"
    TSDLARGUMENT_DEFINITION -> "ARGUMENT_DEFINITION"
    TSDLINTERFACE -> "INTERFACE"
    TSDLUNION -> "UNION"
    TSDLENUM -> "ENUM"
    TSDLENUM_VALUE -> "ENUM_VALUE"
    TSDLINPUT_OBJECT -> "INPUT_OBJECT"
    TSDLINPUT_FIELD_DEFINITION -> "INPUT_FIELD_DEFINITION"

--------------------------------------------------------------------------------
-- Type System Extension Definitions

-- TODO:
data TypeSystemExtension = TypeSystemExtension
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty TypeSystemExtension where
  pretty _ = "TODO Type System Extensions"

--------------------------------------------------------------------------------
-- Executable Definitions

data ExecutableDefinition = OpDef OperationDefinition | FragDef FragmentDefinition
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty ExecutableDefinition where
  pretty = \case
    OpDef od -> pretty od
    FragDef fd -> pretty fd

-- | There are three types of operations that GraphQL models:
--
-- * query – a read-only fetch.
-- * mutation – a write followed by a fetch.
-- * subscription – a long-lived request that fetches data in response to source events.
--
-- Each operation is represented by an optional operation name and a selection set.
data OperationDefinition = OperationDefinition
  { opType :: OperationType,
    opName :: Maybe Name,
    opVariables :: Maybe VariablesDefinition,
    opDirectives :: Directives,
    opSelectionSet :: SelectionSet
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty OperationDefinition where
  pretty OperationDefinition {..} =
    sep
      [ pretty opType,
        pretty opName,
        pretty opVariables,
        pretty opDirectives,
        pretty opSelectionSet
      ]

--------------------------------------------------------------------------------
-- Fragments

-- | Fragments are the primary unit of composition in GraphQL.
--
-- Fragments allow for the reuse of common repeated selections of
-- 'Field's, reducing duplicated text in the 'Document'.
data FragmentDefinition = FragmentDefinition
  { fragName :: FragmentName,
    fragTypeCondition :: TypeCondition,
    fragDirectives :: Directives,
    fragSelectionSet :: SelectionSet
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty FragmentDefinition where
  pretty FragmentDefinition {..} =
    sep
      [ "fragment",
        pretty fragName,
        pretty fragTypeCondition,
        pretty fragDirectives,
        pretty fragSelectionSet
      ]

-- | Fragments are consumed by using the spread operator @(...)@. All
-- 'Field's selected by the fragment will be added to the 'Field'
-- selection at the same level as the fragment invocation. This
-- happens through multiple levels of fragment spreads.
data FragmentSpread = FragmentSpread
  { fsName :: FragmentName,
    fsDirectives :: Directives
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty FragmentSpread where
  pretty FragmentSpread {..} =
    sep
      [ "..." <> pretty fsName,
        pretty fsDirectives
      ]

-- | 'InlineFragment' can be used directly within a 'Selection' to
-- condition upon a type condition when querying against an interface
-- or union.
data InlineFragment = InlineFragment
  { ifTypeCondition :: Maybe TypeCondition,
    ifDirectives :: Directives,
    ifSelectionSet :: SelectionSet
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty InlineFragment where
  pretty InlineFragment {..} =
    sep
      [ "..." <> maybe mempty pretty ifTypeCondition,
        pretty ifDirectives,
        pretty ifSelectionSet
      ]

newtype FragmentName = FragmentName {unFragmentName :: Name}
  deriving stock (Lift)
  deriving newtype (Eq, Ord, Show, Read, Hashable, NFData)

instance Pretty FragmentName where
  pretty FragmentName {..} = pretty unFragmentName

--------------------------------------------------------------------------------
-- Fields

-- | A 'Field' describes one discrete piece of information available
-- to request within a 'SelectionSet'.
--
-- 'Field's are conceptually functions which return values, and
-- occasionally accept 'Arguments' which alter their behavior. These
-- 'Arguments' often map directly to function arguments within a
-- GraphQL service’s implementation.
data Field = Field
  { fieldAlias :: Maybe Name,
    fieldName :: Name,
    fieldArguments :: Arguments,
    fieldDirectives :: Directives,
    fieldSelectionSet :: Maybe SelectionSet
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty Field where
  pretty Field {..} =
    let fieldAlias' = maybe "" ((<> ":") . pretty) fieldAlias
     in sep
          [ fieldAlias',
            pretty fieldName,
            pretty fieldArguments,
            pretty fieldDirectives,
            pretty fieldSelectionSet
          ]

--------------------------------------------------------------------------------
-- Selections

-- | The set of information selected by an operation.
newtype SelectionSet = SelectionSet (NE.NonEmpty Selection)
  deriving stock (Lift, Generic)
  deriving newtype (Eq, Ord, Show, Read, Semigroup)
  deriving anyclass (NFData)

instance Pretty SelectionSet where
  pretty (SelectionSet set) = encloseSep "{" "}" line (f <$> NE.toList set)
    where
      f :: Selection -> Doc ann
      f = \case
        Left (Left field) -> pretty field
        Left (Right fragmentSpread) -> pretty fragmentSpread
        Right inlineFragment -> pretty inlineFragment

-- TODO: Remove Eithers
type Selection = Field \/ FragmentSpread \/ InlineFragment

--------------------------------------------------------------------------------
-- Values

newtype EnumValue = EnumValue {unEnum :: Text}
  deriving stock (Generic, Lift)
  deriving newtype (Eq, Ord, Show, Read)
  deriving anyclass (Hashable, NFData)

instance Pretty EnumValue where
  pretty = pretty . unEnum

data Value
  = VVar Text
  | VNull
  | VInt Integer
  | VFloat Scientific
  | VString Text
  | VBoolean Bool
  | VEnum EnumValue
  | VList [Value]
  | VObject (HashMap Name Value)
  deriving stock (Eq, Ord, Show, Read, Generic, Lift)
  deriving anyclass (Hashable, NFData)

instance Pretty Value where
  pretty = \case
    VVar var -> "$" <> pretty var
    VNull -> "null"
    VInt n -> pretty n
    VFloat sci -> pretty $ show sci
    VString txt -> "\"" <> pretty txt <> "\""
    VBoolean True -> "true"
    VBoolean False -> "false"
    VEnum ev -> pretty ev
    VList xs -> pretty xs
    VObject obj -> ob $ Map.toList obj
    where
      ob :: (Pretty a, Pretty b) => [(a, b)] -> Doc ann
      ob xs =
        let xs' = (\(n, v) -> pretty n <> ":" <+> pretty v) <$> xs
         in group $
              encloseSep
                (flatAlt "{ " "{")
                (flatAlt " }" "}")
                " "
                xs'

--------------------------------------------------------------------------------
-- Misc

-- | Documentation is a first-class feature of GraphQL type
-- systems. To ensure the documentation of a GraphQL service remains
-- consistent with its capabilities, descriptions of GraphQL
-- definitions are provided alongside their definitions and made
-- available via introspection.
newtype Description = Description {unDescription :: Text}
  deriving stock (Eq, Ord, Read, Lift, Generic)
  deriving anyclass (NFData)

-- NOTE: We must escape newlines in Descriptions for our golden test
-- read/show ISO. This show instance must never be used when pretty
-- printing.
instance Show Description where
  show (Description txt) =
    "Description\n" <> "{ unDescription =" <> show (T.replace "\n" "\\n" txt) <> "}"

instance Pretty Description where
  -- TODO(SOLOMON): Distinguish block from literal string quotes
  pretty Description {..} = dquote <> dquote <> dquote <> pretty unDescription <> dquote <> dquote <> dquote

data OperationType = Query | Mutation | Subscription
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty OperationType where
  pretty = \case
    Query -> "query"
    Mutation -> "mutation"
    Subscription -> "subscription"

newtype Name = Name {unName :: Text}
  deriving stock (Lift)
  deriving newtype (Eq, Ord, Show, Read, Hashable, NFData)

instance Pretty Name where
  pretty = pretty . unName

mkName :: Text -> Maybe Name
mkName text =
  T.uncons text >>= \(first, body) ->
    if matchFirst first && T.all matchBody body
      then Just (Name text)
      else Nothing
  where
    matchFirst c = c == '_' || C.isAsciiUpper c || C.isAsciiLower c
    matchBody c = c == '_' || C.isAsciiUpper c || C.isAsciiLower c || C.isDigit c

newtype VariablesDefinition = VariablesDefinition {variablesDefinition :: NE.NonEmpty VariableDefinition}
  deriving stock (Lift, Generic)
  deriving newtype (Eq, Ord, Show, Read, Semigroup)
  deriving anyclass (NFData)

instance Pretty VariablesDefinition where
  pretty (VariablesDefinition vars) =
    tupled (NE.toList $ fmap pretty vars)

data VariableDefinition = VariableDefinition
  { varName :: Name,
    varType :: Type,
    varDefaultValue :: Maybe Value,
    varDirectives :: Directives
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty VariableDefinition where
  pretty VariableDefinition {..} =
    sep
      [ "$" <> pretty varName <> ":",
        pretty varType,
        maybe mempty ((space <>) . pretty) varDefaultValue,
        pretty varDirectives
      ]

newtype Arguments = Arguments {unArguments :: HashMap Name Value}
  deriving stock (Generic, Lift)
  deriving newtype (Eq, Ord, Show, Read, Semigroup, Monoid)
  deriving anyclass (Hashable, NFData)

instance Pretty Arguments where
  pretty Arguments {..} =
    if unArguments == mempty
      then mempty
      else tupled $ fmap (\(n, v) -> pretty n <> ":" <+> pretty v) (Map.toList unArguments)

newtype TypeCondition = TypeCondition {unTypeCondition :: Name}
  deriving stock (Generic, Lift)
  deriving stock (Eq, Ord, Show, Read)
  deriving anyclass (Hashable, NFData)

instance Pretty TypeCondition where
  pretty TypeCondition {..} =
    "on" <+> pretty unTypeCondition

data Type = NamedType Name | ListType Type | NonNullType Type
  deriving stock (Generic, Lift)
  deriving stock (Eq, Ord, Show, Read)
  deriving anyclass (Hashable, NFData)

instance Pretty Type where
  pretty = \case
    NamedType ty -> pretty ty
    ListType xs -> "[" <> pretty xs <> "]"
    NonNullType ty -> pretty ty <> "!"

newtype Directives = Directives {unDirectives :: [Directive]}
  deriving stock (Lift, Generic)
  deriving newtype (Eq, Ord, Show, Read, Semigroup, Monoid)
  deriving anyclass (NFData)

instance Pretty Directives where
  pretty Directives {..} =
    fold (punctuate " " $ fmap pretty unDirectives)

data Directive = Directive
  { dName :: Name,
    dArguments :: Maybe Arguments
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty Directive where
  pretty Directive {..} = "@" <> pretty dName <> pretty dArguments

--------------------------------------------------------------------------------
-- Pretty helpers

renderDoc :: Doc ann -> Text
renderDoc = renderStrict . layoutPretty defaultLayoutOptions

renderPretty :: Pretty a => a -> Text
renderPretty = renderDoc . pretty

renderPrettyBS :: Pretty a => a -> B.ByteString
renderPrettyBS = TE.encodeUtf8 . renderDoc . pretty

renderVect :: Pretty a => V.Vector a -> Text
renderVect = renderDoc . foldMap pretty

renderBL :: BL.ByteString -> Text
renderBL = TE.decodeUtf8 . BL.toStrict

right :: Either e a -> a
right (Right a) = a
