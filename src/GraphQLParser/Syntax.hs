{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}

-- | GraphQL IR
module GraphQLParser.Syntax where

import Control.DeepSeq (NFData)
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.List.NonEmpty qualified as NE
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)
import Prettyprinter (Doc, Pretty (pretty), defaultLayoutOptions, layoutPretty)
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

-- | A GraphQL Document describes a complete file or request string
-- operated on by a GraphQL service or client. A document contains
-- multiple definitions, either executable or representative of a
-- GraphQL type system.
type GraphQLDocument = Document (ExecutableDefinition \/ TypeSystemDefinitionOrExtension)

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
type TypeSystemDefinitionOrExtension = TypeSystemDefinition \/ TypeSystemExtension

type TypeSystemDefinition = SchemaDefinition \/ TypeDefinition \/ DirectiveDefinition

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
    sdRootOperations :: [RootOperationTypeDefinition]
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

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

--------------------------------------------------------------------------------
-- Type Definitions

-- | The fundamental unit of any GraphQL Schema is the type.
type TypeDefinition =
  ScalarTypeDefinition \/ ObjectTypeDefinition \/ InterfaceTypeDefinition \/ UnionTypeDefinition \/ EnumTypeDefinition \/ InputObjectTypeDefinition

data ScalarTypeDefinition = ScalarTypeDefinition
  { scalarDescription :: Maybe Description,
    scalarName :: Name,
    scalarDirectives :: Directives
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

-- | GraphQL operations are hierarchical and composed, describing a
-- tree of information. While Scalar types describe the leaf values of
-- these hierarchical operations, Objects describe the intermediate
-- levels.
data ObjectTypeDefinition = ObjectTypeDefinition
  { objectDescription :: Maybe Description,
    objectName :: Name,
    objectInterfaces :: [Name],
    objectDirectives :: Directives,
    objectFields :: [FieldDefinition]
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

data FieldDefinition = FieldDefinition
  { fieldDefDescription :: Maybe Description,
    fieldDefName :: Name,
    fieldDefArgumentsDef :: ArgumentsDefinition,
    fieldDefType :: Type,
    fieldDefDirectives :: Directives
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

-- | GraphQL interfaces represent a list of named fields and their
-- arguments. GraphQL objects and interfaces can then implement these
-- interfaces which requires that the implementing type will define
-- all fields defined by those interfaces.
data InterfaceTypeDefinition = InterfaceTypeDefinition
  { interfaceDescription :: Maybe Description,
    interfaceName :: Name,
    interfaceInterfaces :: [Name],
    interfaceDirectives :: Directives,
    interfaceFields :: [FieldDefinition]
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

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

data EnumValueDefinition = EnumValueDefinition
  { evDescription :: Maybe Description,
    evName :: Name,
    evDirectives :: Directives
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

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

newtype InputFieldsDefinition = InputFieldsDefinition {unInputFieldsDefinition :: [InputValueDefinition]}
  deriving stock (Lift, Generic)
  deriving newtype (Eq, Ord, Show, Read, Semigroup, Monoid)
  deriving anyclass (NFData)


newtype ArgumentsDefinition = ArgumentsDefinition {unArgumentsDefinition :: [InputValueDefinition]}
  deriving stock (Lift, Generic)
  deriving newtype (Eq, Ord, Show, Read, Semigroup, Monoid)
  deriving anyclass (NFData)

data InputValueDefinition = InputValueDefinition
  { inputValueDescription :: Maybe Description,
    inputValueName :: Name,
    inputValueType :: Type,
    inputValueDefault :: Maybe Value,
    inputValueDirectives :: Directives
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

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

type DirectiveLocation = ExecutableDirectiveLocation \/ TypeSystemDirectiveLocation

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

--------------------------------------------------------------------------------
-- Type System Extension Definitions

-- TODO:
data TypeSystemExtension = TypeSystemExtension
  deriving stock (Eq, Ord, Show, Read, Lift)

--------------------------------------------------------------------------------
-- Executable Definitions

type ExecutableDefinition = OperationDefinition \/ FragmentDefinition

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
    opVariables :: [VariableDefinition],
    opDirectives :: Directives,
    opSelectionSet :: SelectionSet
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

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

newtype FragmentName = FragmentName {unFragmentName :: Name}
  deriving stock (Lift)
  deriving newtype (Eq, Ord, Show, Read, Hashable, NFData)

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

--------------------------------------------------------------------------------
-- Selections

-- | The set of information selected by an operation.
newtype SelectionSet = SelectionSet (NE.NonEmpty Selection)
  deriving stock (Lift, Generic)
  deriving newtype (Eq, Ord, Show, Read, Semigroup)
  deriving anyclass (NFData)

type Selection = Field \/ FragmentSpread \/ InlineFragment

--------------------------------------------------------------------------------
-- Values

newtype EnumValue = EnumValue {unEnum :: Text}
  deriving stock (Generic, Lift)
  deriving newtype (Eq, Ord, Show, Read)
  deriving anyclass (Hashable, NFData)

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

data OperationType = Query | Mutation | Subscription
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

newtype Name = Name {unName :: Text}
  deriving stock (Lift)
  deriving newtype (Eq, Ord, Show, Read, Hashable, NFData)

data VariableDefinition = VariableDefinition
  { varName :: Name,
    varType :: Type,
    varDefaultValue :: Maybe Value,
    varDirectives :: Directives
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

newtype Arguments = Arguments {unArguments :: HashMap Name Value}
  deriving stock (Generic, Lift)
  deriving newtype (Eq, Ord, Show, Read, Semigroup, Monoid)
  deriving anyclass (Hashable, NFData)

newtype TypeCondition = TypeCondition {unTypeCondition :: Name}
  deriving stock (Generic, Lift)
  deriving stock (Eq, Ord, Show, Read)
  deriving anyclass (Hashable, NFData)

data Type = NamedType Name | ListType Type | NonNullType Type
  deriving stock (Generic, Lift)
  deriving stock (Eq, Ord, Show, Read)
  deriving anyclass (Hashable, NFData)


newtype Directives = Directives {unDirectives :: [Directive]}
  deriving stock (Lift, Generic)
  deriving newtype (Eq, Ord, Show, Read, Semigroup, Monoid)
  deriving anyclass (NFData)
data Directive = Directive
  { dName :: Name,
    dArguments :: Maybe Arguments
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

--------------------------------------------------------------------------------
-- Pretty helpers

renderDoc :: Doc ann -> Text
renderDoc = renderStrict . layoutPretty defaultLayoutOptions

renderPretty :: Pretty a => a -> Text
renderPretty = renderDoc . pretty

renderVect :: Pretty a => V.Vector a -> Text
renderVect = renderDoc . foldMap pretty

renderBL :: BL.ByteString -> Text
renderBL = TE.decodeUtf8 . BL.toStrict
