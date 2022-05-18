{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | GraphQL IR
module GraphQLParser.IR where

import Control.DeepSeq (NFData)
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.List.NonEmpty qualified as NE
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Prettyprinter (Doc, Pretty (pretty), defaultLayoutOptions, layoutPretty)

type a \/ b = Either a b

--------------------------------------------------------------------------------

-- | A GraphQL 'Document' describes a complete file or request string
-- operated on by a GraphQL service or client. A 'Document' contains
-- multiple 'Definition's, either executable or representative of a
-- GraphQL type system.
newtype Document definition = Document [definition]
  deriving newtype (Eq, Ord, Show, Read)

-- | A 'Document' containing strictly '[ExecutableDefinition]' where at
-- least one value is a 'OperationDefinition' is considered an
-- Executable Document.
--
-- 'Document's are only executable by a GraphQL service if they are
-- 'ExecutableDocument'.
type ExecutableDocument = Document ExecutableDefinition

--------------------------------------------------------------------------------
-- Type System Definitions

-- | A 'Document' which contains 'TypeSystemDefinitionOrExtension' must
-- not be executed; GraphQL execution services which receive a
-- 'Document' containing these should return a descriptive error.
--type TypeSystemDocument = Document TypeSystemDefinitionOrExtension

--type TypeSystemDefinitionOrExtension = TypeSystemDefinition \/ TypeSystemExtension

type TypeSystemDocument = SchemaDefinition \/ TypeDefinition \/ DirectiveDefinition

-- TODO:
data SchemaDefinition = SchemaDefinition
  deriving stock (Eq, Ord, Show, Read)

-- TODO:
data TypeDefinition = TypeDefinition
  deriving stock (Eq, Ord, Show, Read)

-- TODO:
data DirectiveDefinition = DirectiveDefinition
  deriving stock (Eq, Ord, Show, Read)

-- TODO:
data TypeSystemExtension = TypeSystemExtension
  deriving stock (Eq, Ord, Show, Read)

------------------------------------------------------- ------------------------
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
  deriving stock (Eq, Ord, Show, Read)

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
  deriving stock (Eq, Generic, Ord, Show, Read)

-- | Fragments are consumed by using the spread operator @(...)@. All
-- 'Field's selected by the fragment will be added to the 'Field'
-- selection at the same level as the fragment invocation. This
-- happens through multiple levels of fragment spreads.
data FragmentSpread = FragmentSpread
  { fsName :: FragmentName,
    fsDirectives :: [Directive]
  }
  deriving stock (Eq, Generic, Ord, Show, Read)

-- | 'InlineFragment' can be used directly within a 'Selection' to
-- condition upon a type condition when querying against an interface
-- or union.
data InlineFragment = InlineFragment
  { ifTypeCondition :: Maybe Name,
    ifDirectives :: [Directive],
    ifSelectionSet :: SelectionSet
  }
  deriving stock (Eq, Generic, Ord, Show, Read)

newtype FragmentName = FragmentName {unFragmentName :: Name}
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
  deriving stock (Eq, Ord, Show, Read)

--------------------------------------------------------------------------------
-- Selections

-- | The set of information selected by an operation.
newtype SelectionSet = SelectionSet (NE.NonEmpty Selection)
  deriving newtype (Eq, Ord, Show, Read, Semigroup)

type Selection = Field \/ FragmentSpread \/ InlineFragment

--------------------------------------------------------------------------------
-- Values

newtype EnumValue = EnumValue {unEnum :: Text}
  deriving stock (Generic)
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
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Hashable, NFData)

--------------------------------------------------------------------------------
-- Misc

data OperationType = Query | Mutation | Subscription
  deriving stock (Eq, Ord, Show, Read)

newtype Name = Name {unName :: Text}
  deriving newtype (Eq, Ord, Show, Read, Hashable, NFData)

data VariableDefinition = VariableDefinition
  { varName :: Name,
    varType :: Type,
    varDefaultValue :: Maybe Value,
    varDirectives :: [Directive]
  }
  deriving stock (Eq, Ord, Show, Read)

newtype Arguments = Arguments {unArguments :: HashMap Name Value}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Read, Semigroup, Monoid)
  deriving anyclass (Hashable, NFData)

data Type = NamedType Name | ListType Type | NonNullType Type
  deriving stock (Generic)
  deriving stock (Eq, Ord, Show, Read)
  deriving anyclass (Hashable, NFData)

data Directive = Directive
  { name :: Name,
    arguments :: Arguments
  }
  deriving stock (Eq, Ord, Show, Read)

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
