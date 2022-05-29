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
  deriving newtype (Eq, Ord, Show, Read)

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
    sdDirectives :: [Directive],
    sdRootOperations :: [RootOperationTypeDefinition]
  }
  deriving stock (Eq, Ord, Show, Read, Lift)

-- | A 'SchemaDefiniton' defines the initial root operation type for
-- each kind of operation it supports: @Query@, @Mutation@, and
-- @Subscription@; this determines the place in the type system where
-- those operations begin.
data RootOperationTypeDefinition = RootOperationTypeDefinition
  { roType :: OperationType,
    roTypeName :: Name
  }
  deriving stock (Eq, Ord, Show, Read, Lift)

--------------------------------------------------------------------------------
-- Type Definitions

-- | The fundamental unit of any GraphQL Schema is the type.
type TypeDefinition =
  ScalarTypeDefinition \/ ObjectTypeDefinition \/ InterfaceTypeDefinition \/ UnionTypeDefinition \/ EnumTypeDefinition \/ InputObjectTypeDefinition

data ScalarTypeDefinition = ScalarTypeDefinition
  { scalarDescription :: Maybe Description,
    scalarName :: Name,
    scalarDirectives :: [Directive]
  }
  deriving stock (Eq, Ord, Show, Read, Lift)

-- | GraphQL operations are hierarchical and composed, describing a
-- tree of information. While Scalar types describe the leaf values of
-- these hierarchical operations, Objects describe the intermediate
-- levels.
data ObjectTypeDefinition = ObjectTypeDefinition
  { objectDescription :: Maybe Description,
    objectName :: Name,
    objectInterfaces :: [Name],
    objectDirectives :: [Directive],
    objectFields :: [FieldDefinition]
  }
  deriving stock (Eq, Ord, Show, Read, Lift)

data FieldDefinition = FieldDefinition
  { fieldDefDescription :: Maybe Description,
    fieldDefName :: Name,
    fieldDefArgumentsDef :: [InputValueDefinition],
    fieldDefType :: Type,
    fieldDefDirectives :: [Directive]
  }
  deriving stock (Eq, Ord, Show, Read, Lift)

-- | GraphQL interfaces represent a list of named fields and their
-- arguments. GraphQL objects and interfaces can then implement these
-- interfaces which requires that the implementing type will define
-- all fields defined by those interfaces.
data InterfaceTypeDefinition = InterfaceTypeDefinition
  { interfaceDescription :: Maybe Description,
    interfaceName :: Name,
    interfaceInterfaces :: [Name],
    interfaceDirectives :: [Directive],
    interfaceFields :: [FieldDefinition]
  }
  deriving stock (Eq, Ord, Show, Read, Lift)

-- | GraphQL Unions represent an object that could be one of a list of
-- GraphQL Object types, but provides for no guaranteed fields between
-- those types. They also differ from interfaces in that Object types
-- declare what interfaces they implement, but are not aware of what
-- unions contain them.
data UnionTypeDefinition = UnionTypeDefinition
  { unionDescription :: Maybe Description,
    unionName :: Name,
    unionDirectives :: [Directive],
    unionMemberTypes :: [Name]
  }
  deriving stock (Eq, Ord, Show, Read, Lift)

-- | GraphQL Enum types, like Scalar types, also represent leaf values
-- in a GraphQL type system. However Enum types describe the set of
-- possible values.
data EnumTypeDefinition = EnumTypeDefinition
  { enumDescription :: Maybe Description,
    enumName :: Name,
    enumDirectives :: [Directive],
    enumValues :: [EnumValueDefinition]
  }
  deriving stock (Eq, Ord, Show, Read, Lift)

data EnumValueDefinition = EnumValueDefinition
  { enumValueDescription :: Maybe Description,
    enumValue :: Name,
    enumValueDirectives :: [Directive]
  }
  deriving stock (Eq, Ord, Show, Read, Lift)

-- | A GraphQL Input Object defines a set of input fields; the input
-- fields are either scalars, enums, or other input objects. This
-- allows arguments to accept arbitrarily complex structs.
data InputObjectTypeDefinition = InputObjectTypeDefinition
  { inputDescription :: Maybe Description,
    inputName :: Name,
    inputDirectives :: [Directive],
    inputValues :: [InputValueDefinition]
  }
  deriving stock (Eq, Ord, Show, Read, Lift)

data InputValueDefinition = InputValueDefinition
  { inputValueDescription :: Maybe Description,
    inputValue :: Name,
    inputType :: Type,
    inputDefaultValue :: Maybe Value,
    inputValueDirectives :: [Directive]
  }
  deriving stock (Eq, Ord, Show, Read, Lift)

--------------------------------------------------------------------------------
-- Directive Definitions

data DirectiveDefinition = DirectiveDefinition
  { dirDefDescription :: Maybe Description,
    dirDefName :: Name,
    dirDefArguments :: [InputValueDefinition],
    dirDefLocations :: [DirectiveLocation]
  }
  deriving stock (Eq, Ord, Show, Read, Lift)

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
  deriving stock (Eq, Generic, Ord, Show, Read, Lift)

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
  deriving stock (Eq, Generic, Ord, Show, Read, Lift)

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
    opDirectives :: [Directive],
    opSelectionSet :: SelectionSet
  }
  deriving stock (Eq, Ord, Show, Read, Lift)

--------------------------------------------------------------------------------
-- Fragments

-- | Fragments are the primary unit of composition in GraphQL.
--
-- Fragments allow for the reuse of common repeated selections of
-- 'Field's, reducing duplicated text in the 'Document'.
data FragmentDefinition = FragmentDefinition
  { fragName :: FragmentName,
    fragTypeCondition :: Type,
    fragDirectives :: [Directive],
    fragSelectionSet :: SelectionSet
  }
  deriving stock (Eq, Generic, Ord, Show, Read, Lift)

-- | Fragments are consumed by using the spread operator @(...)@. All
-- 'Field's selected by the fragment will be added to the 'Field'
-- selection at the same level as the fragment invocation. This
-- happens through multiple levels of fragment spreads.
data FragmentSpread = FragmentSpread
  { fsName :: FragmentName,
    fsDirectives :: [Directive]
  }
  deriving stock (Eq, Generic, Ord, Show, Read, Lift)

-- | 'InlineFragment' can be used directly within a 'Selection' to
-- condition upon a type condition when querying against an interface
-- or union.
data InlineFragment = InlineFragment
  { ifTypeCondition :: Maybe Name,
    ifDirectives :: [Directive],
    ifSelectionSet :: SelectionSet
  }
  deriving stock (Eq, Generic, Ord, Show, Read, Lift)

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
    fieldDirectives :: [Directive],
    fieldSelectionSet :: Maybe SelectionSet
  }
  deriving stock (Eq, Ord, Show, Read, Lift)

--------------------------------------------------------------------------------
-- Selections

-- | The set of information selected by an operation.
newtype SelectionSet = SelectionSet (NE.NonEmpty Selection)
  deriving stock (Lift)
  deriving newtype (Eq, Ord, Show, Read, Semigroup)

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
  deriving stock (Eq, Ord, Show, Read, Lift)

data OperationType = Query | Mutation | Subscription
  deriving stock (Eq, Ord, Show, Read, Lift)

newtype Name = Name {unName :: Text}
  deriving stock (Lift)
  deriving newtype (Eq, Ord, Show, Read, Hashable, NFData)

data VariableDefinition = VariableDefinition
  { varName :: Name,
    varType :: Type,
    varDefaultValue :: Maybe Value,
    varDirectives :: [Directive]
  }
  deriving stock (Eq, Ord, Show, Read, Lift)

newtype Arguments = Arguments {unArguments :: HashMap Name Value}
  deriving stock (Generic, Lift)
  deriving newtype (Eq, Ord, Show, Read, Semigroup, Monoid)
  deriving anyclass (Hashable, NFData)

data Type = NamedType Name | ListType Type | NonNullType Type
  deriving stock (Generic, Lift)
  deriving stock (Eq, Ord, Show, Read)
  deriving anyclass (Hashable, NFData)

data Directive = Directive
  { name :: Name,
    arguments :: Arguments
  }
  deriving stock (Eq, Ord, Show, Read, Lift)

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
