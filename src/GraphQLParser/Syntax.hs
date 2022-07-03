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
import GraphQLParser.Span qualified as S
import Language.Haskell.TH.Syntax (Lift)
import Prettyprinter (Doc, Pretty (..), defaultLayoutOptions, dquote, encloseSep, flatAlt, group, indent, layoutPretty, line, punctuate, sep, tupled, vsep, (<+>))
import Prettyprinter.Render.Text (renderStrict)

--------------------------------------------------------------------------------

-- | A GraphQL 'Document' describes a complete file or request string
-- operated on by a GraphQL service or client. A 'Document' contains
-- multiple 'Definition's, either executable or representative of a
-- GraphQL type system.
newtype Document = Document {getDefinitions :: [Definition]}
  deriving stock (Lift)
  deriving newtype (Eq, Ord, Show, Read, NFData)

instance Pretty Document where
  pretty (Document defs) = foldMap ((<> line) . pretty) defs

data Definition = DefinitionExecutable ExecutableDefinition | DefinitionTypeSystem TypeSystemDefinition
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty Definition where
  pretty = \case
    DefinitionExecutable ed -> pretty ed
    DefinitionTypeSystem ts -> pretty ts

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
  = TypeSystemDefinitionSchema SchemaDefinition
  | TypeSystemDefinitionType TypeDefinition
  | TypeSystemDefinitionDirective DirectiveDefinition
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty TypeSystemDefinition where
  pretty = \case
    TypeSystemDefinitionSchema sd -> pretty sd
    TypeSystemDefinitionType ty -> pretty ty
    TypeSystemDefinitionDirective dd -> pretty dd

--------------------------------------------------------------------------------
-- Schema Definitions

-- | A GraphQL service’s collective type system capabilities are
-- referred to as that service’s "schema". A schema is defined in
-- terms of the types and directives it supports as well as the root
-- operation types for each kind of operation: query, mutation, and
-- subscription; this determines the place in the type system where
-- those operations begin.
data SchemaDefinition = SchemaDefinition
  { _sdSpan :: S.Span,
    _sdDescription :: Maybe Description,
    _sdDirectives :: Maybe Directives,
    _sdRootOperationTypeDefinitions :: RootOperationTypesDefinition
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance S.Located SchemaDefinition where
  locate = _sdSpan

instance Pretty SchemaDefinition where
  pretty SchemaDefinition {..} =
    sep
      [ pretty _sdDescription,
        "schema",
        pretty _sdDirectives,
        pretty _sdRootOperationTypeDefinitions
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
  { _rotdSpan :: S.Span,
    _rotdOperationType :: OperationType,
    _rotdNamedType :: Name
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance S.Located RootOperationTypeDefinition where
  locate = _rotdSpan

instance Pretty RootOperationTypeDefinition where
  pretty RootOperationTypeDefinition {..} =
    sep [pretty _rotdOperationType <> ":", pretty _rotdNamedType]

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
  { _stSpan :: S.Span,
    _stDescription :: Maybe Description,
    _stName :: Name,
    _stDirectives :: Maybe Directives
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance S.Located ScalarTypeDefinition where
  locate = _stSpan

instance Pretty ScalarTypeDefinition where
  pretty ScalarTypeDefinition {..} =
    sep
      [ pretty _stDescription,
        "scalar",
        pretty _stName,
        pretty _stDirectives
      ]

-- | GraphQL operations are hierarchical and composed, describing a
-- tree of information. While Scalar types describe the leaf values of
-- these hierarchical operations, Objects describe the intermediate
-- levels.
data ObjectTypeDefinition = ObjectTypeDefinition
  { _otdSpan :: S.Span,
    _otdDescription :: Maybe Description,
    _otdName :: Name,
    _otdImplementsInterfaces :: Maybe ImplementsInterfaces,
    _otdDirectives :: Maybe Directives,
    _otdFieldsDefinition :: Maybe FieldsDefinition
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance S.Located ObjectTypeDefinition where
  locate = _otdSpan

instance Pretty ObjectTypeDefinition where
  pretty ObjectTypeDefinition {..} =
    sep
      [ pretty _otdDescription,
        "type",
        pretty _otdName,
        pretty _otdImplementsInterfaces,
        pretty _otdDirectives,
        pretty _otdFieldsDefinition
      ]

data FieldsDefinition = FieldsDefinition
  { _fdSpan :: S.Span,
    _fdDefinitions :: NE.NonEmpty FieldDefinition
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance S.Located FieldsDefinition where
  locate = _fdSpan

instance Pretty FieldsDefinition where
  pretty FieldsDefinition {..} =
    encloseSep "{" "}" line (foldr (\x xs -> pretty x : xs) [] _fdDefinitions)

data FieldDefinition = FieldDefinition
  { _fldSpan :: S.Span,
    _fldDescription :: Maybe Description,
    _fldName :: Name,
    _fldArgumentsDefinition :: Maybe ArgumentsDefinition,
    _fldType :: Type,
    _fldDirectives :: Maybe Directives
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance S.Located FieldDefinition where
  locate = _fldSpan

instance Pretty FieldDefinition where
  pretty FieldDefinition {..} =
    sep
      [ pretty _fldDescription,
        pretty _fldName,
        pretty _fldArgumentsDefinition,
        ":",
        pretty _fldType,
        pretty _fldDirectives
      ]

-- | GraphQL interfaces represent a list of named fields and their
-- arguments. GraphQL objects and interfaces can then implement these
-- interfaces which requires that the implementing type will define
-- all fields defined by those interfaces.
data InterfaceTypeDefinition = InterfaceTypeDefinition
  { _itSpan :: S.Span,
    _itDescription :: Maybe Description,
    _itName :: Name,
    _itInterfaces :: Maybe ImplementsInterfaces,
    _itDirectives :: Maybe Directives,
    _itFields :: Maybe FieldsDefinition
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance S.Located InterfaceTypeDefinition where
  locate = _itSpan

instance Pretty InterfaceTypeDefinition where
  pretty InterfaceTypeDefinition {..} =
    sep
      [ pretty _itDescription,
        "interface",
        pretty _itName,
        pretty _itInterfaces,
        pretty _itDirectives,
        pretty _itFields
      ]

-- TODO: Use a Vector
data ImplementsInterfaces = ImplementsInterfaces
  { _iiSpan :: S.Span,
    _iiInterfaces :: NE.NonEmpty Name
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance S.Located ImplementsInterfaces where
  locate = _iiSpan

instance Pretty ImplementsInterfaces where
  pretty ImplementsInterfaces {..} =
    "implements" <+> foldMap (\n -> "&" <+> pretty n) _iiInterfaces

--"implements" <+> foldl (\acc n -> "&" <+> pretty n <+> acc) "" _iiInterfaces

-- | GraphQL Unions represent an object that could be one of a list of
-- GraphQL Object types, but provides for no guaranteed fields between
-- those types. They also differ from interfaces in that Object types
-- declare what interfaces they implement, but are not aware of what
-- unions contain them.
data UnionTypeDefinition = UnionTypeDefinition
  { _utSpan :: S.Span,
    _utdDescription :: Maybe Description,
    _utdName :: Name,
    _utdDirectives :: Maybe Directives,
    _utdMemberTypes :: [Name]
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance S.Located UnionTypeDefinition where
  locate = _utSpan

instance Pretty UnionTypeDefinition where
  pretty UnionTypeDefinition {..} =
    sep
      [ pretty _utdDescription,
        "union",
        pretty _utdName,
        pretty _utdDirectives,
        "=",
        foldMap (\n -> "|" <+> pretty n <> line) _utdMemberTypes
      ]

-- | GraphQL Enum types, like Scalar types, also represent leaf values
-- in a GraphQL type system. However Enum types describe the set of
-- possible values.
data EnumTypeDefinition = EnumTypeDefinition
  { _etdSpan :: S.Span,
    _etdDescription :: Maybe Description,
    _etdName :: Name,
    _etdDirectives :: Maybe Directives,
    _etdValueDefinitions :: [EnumValueDefinition]
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance S.Located EnumTypeDefinition where
  locate = _etdSpan

instance Pretty EnumTypeDefinition where
  pretty EnumTypeDefinition {..} =
    sep
      [ pretty _etdDescription,
        "enum",
        pretty _etdName,
        pretty _etdDirectives,
        encloseSep "{" "}" line (fmap pretty _etdValueDefinitions)
      ]

data EnumValueDefinition = EnumValueDefinition
  { _evdSpan :: S.Span,
    _evdDescription :: Maybe Description,
    _evdName :: Name,
    _evdDirectives :: Maybe Directives
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance S.Located EnumValueDefinition where
  locate = _evdSpan

instance Pretty EnumValueDefinition where
  pretty EnumValueDefinition {..} =
    sep
      [ pretty _evdDescription,
        pretty _evdName,
        pretty _evdDirectives
      ]

-- | A GraphQL Input Object defines a set of input fields; the input
-- fields are either scalars, enums, or other input objects. This
-- allows arguments to accept arbitrarily complex structs.
data InputObjectTypeDefinition = InputObjectTypeDefinition
  { _iotSpan :: S.Span,
    _iotDescription :: Maybe Description,
    _iotName :: Name,
    _iotDirectives :: Maybe Directives,
    _iotValueDefinitions :: Maybe InputFieldsDefinition
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance S.Located InputObjectTypeDefinition where
  locate = _iotSpan

instance Pretty InputObjectTypeDefinition where
  pretty InputObjectTypeDefinition {..} =
    sep $
      [ pretty _iotDescription,
        "input",
        pretty _iotName,
        pretty _iotDirectives,
        -- TODO: Check printer spacing
        pretty _iotValueDefinitions
      ]

data InputFieldsDefinition = InputFieldsDefinition
  { _ifdSpan :: S.Span,
    _ifdDefinition :: NE.NonEmpty InputValueDefinition
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance S.Located InputFieldsDefinition where
  locate = _ifdSpan

instance Pretty InputFieldsDefinition where
  pretty InputFieldsDefinition {..} =
    encloseSep "{" "}" " " (foldr (\x xs -> pretty x : xs) [] _ifdDefinition)

newtype ArgumentsDefinition = ArgumentsDefinition {unArgumentsDefinition :: NE.NonEmpty InputValueDefinition}
  deriving stock (Lift, Generic)
  deriving newtype (Eq, Ord, Show, Read, Semigroup)
  deriving anyclass (NFData)

instance Pretty ArgumentsDefinition where
  pretty ArgumentsDefinition {..} =
    case unArgumentsDefinition of
      x NE.:| [] -> "(" <> pretty x <> ")"
      xs -> encloseSep "(" ")" " " (foldr (\x acc -> pretty x : acc) [] xs)

data InputValueDefinition = InputValueDefinition
  { _ivdSpan :: S.Span,
    _ivdDescription :: Maybe Description,
    _ivdName :: Name,
    _ivdType :: Type,
    _ivdDefaultValue :: Maybe Value,
    _ivdDirectives :: Maybe Directives
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance S.Located InputValueDefinition where
  locate = _ivdSpan

instance Pretty InputValueDefinition where
  pretty InputValueDefinition {..} =
    sep
      [ pretty _ivdDescription,
        pretty _ivdName <> ":",
        pretty _ivdType,
        pretty _ivdDefaultValue,
        pretty _ivdDirectives
      ]

--------------------------------------------------------------------------------
-- Directive Definitions

data DirectiveDefinition = DirectiveDefinition
  { _ddSpan :: S.Span,
    _ddDescription :: Maybe Description,
    _ddName :: Name,
    _ddArguments :: Maybe ArgumentsDefinition,
    _ddLocations :: [DirectiveLocation]
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance S.Located DirectiveDefinition where
  locate = _ddSpan

instance Pretty DirectiveDefinition where
  pretty DirectiveDefinition {..} =
    sep
      [ pretty _ddDescription,
        "directive",
        "@" <> pretty _ddName,
        pretty _ddArguments,
        "on",
        foldMap (\n -> "|" <+> pretty n <> line) _ddLocations
      ]

data DirectiveLocation = ExecDirLoc S.Span ExecutableDirectiveLocation | TypeSysDirLoc S.Span TypeSystemDirectiveLocation
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance S.Located DirectiveLocation where
  locate = \case
    ExecDirLoc sp _edl -> sp
    TypeSysDirLoc sp _tsdl -> sp

instance Pretty DirectiveLocation where
  pretty = \case
    ExecDirLoc _sp edl -> pretty edl
    TypeSysDirLoc _sp tsdl -> pretty tsdl

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

data ExecutableDefinition
  = ExecutableDefinitionOperation OperationDefinition
  | ExecutableDefinitionFragment FragmentDefinition
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty ExecutableDefinition where
  pretty = \case
    ExecutableDefinitionOperation od -> pretty od
    ExecutableDefinitionFragment fd -> pretty fd

-- | There are three types of operations that GraphQL models:
--
-- * query – a read-only fetch.
-- * mutation – a write followed by a fetch.
-- * subscription – a long-lived request that fetches data in response to source events.
--
-- Each operation is represented by an optional operation name and a selection set.
data OperationDefinition = OperationDefinition
  { _odSpan :: S.Span,
    _odType :: OperationType,
    _odName :: Maybe Name,
    _odVariables :: Maybe VariablesDefinition,
    _odDirectives :: Maybe Directives,
    _odSelectionSet :: SelectionSet
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance S.Located OperationDefinition where
  locate = _odSpan

instance Pretty OperationDefinition where
  pretty OperationDefinition {..} =
    (pretty _odType <-> fmap pretty _odName <-> fmap pretty _odVariables <-> fmap pretty _odDirectives) <+> pretty _odSelectionSet

--------------------------------------------------------------------------------
-- Fragments

-- | Fragments are the primary unit of composition in GraphQL.
--
-- Fragments allow for the reuse of common repeated selections of
-- 'Field's, reducing duplicated text in the 'Document'.
data FragmentDefinition = FragmentDefinition
  { _frdSpan :: S.Span,
    _frdName :: Name,
    _frdTypeCondition :: TypeCondition,
    _frdDirectives :: Maybe Directives,
    _frdSelectionSet :: SelectionSet
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance S.Located FragmentDefinition where
  locate = _frdSpan

instance Pretty FragmentDefinition where
  pretty FragmentDefinition {..} =
    sep
      [ "fragment",
        pretty _frdName,
        pretty _frdTypeCondition,
        pretty _frdDirectives,
        pretty _frdSelectionSet
      ]

-- | Fragments are consumed by using the spread operator @(...)@. All
-- 'Field's selected by the fragment will be added to the 'Field'
-- selection at the same level as the fragment invocation. This
-- happens through multiple levels of fragment spreads.
data FragmentSpread = FragmentSpread
  { _fsSpan :: S.Span,
    _fsName :: Name,
    _fsDirectives :: Maybe Directives
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance S.Located FragmentSpread where
  locate = _fsSpan

instance Pretty FragmentSpread where
  pretty FragmentSpread {..} =
    sep
      [ "..." <> pretty _fsName,
        pretty _fsDirectives
      ]

-- | 'InlineFragment' can be used directly within a 'Selection' to
-- condition upon a type condition when querying against an interface
-- or union.
data InlineFragment = InlineFragment
  { _ifSpan :: S.Span,
    _ifTypeCondition :: Maybe TypeCondition,
    _ifDirectives :: Maybe Directives,
    _ifSelectionSet :: SelectionSet
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance S.Located InlineFragment where
  locate = _ifSpan

instance Pretty InlineFragment where
  pretty InlineFragment {..} =
    sep
      [ "..." <> maybe mempty pretty _ifTypeCondition,
        pretty _ifDirectives,
        pretty _ifSelectionSet
      ]

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
  { _fSpan :: S.Span,
    _fAlias :: Maybe Name,
    _fName :: Name,
    _fArguments :: Maybe Arguments,
    _fDirectives :: Maybe Directives,
    _fSelectionSet :: Maybe SelectionSet
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance S.Located Field where
  locate = _fSpan

instance Pretty Field where
  pretty Field {..} =
    let fieldAlias' = maybe "" ((<> ":") . pretty) _fAlias
     in fieldAlias' <> pretty _fName <-> fmap pretty _fArguments <-> fmap pretty _fDirectives <-> fmap pretty _fSelectionSet

--in sep
--     [ fieldAlias',
--       pretty _fName,
--       pretty _fArguments,
--       pretty _fDirectives,
--       pretty _fSelectionSet
--     ]

--------------------------------------------------------------------------------
-- Selections

-- | The set of information selected by an operation.
newtype SelectionSet = SelectionSet (NE.NonEmpty Selection)
  deriving stock (Lift, Generic)
  deriving newtype (Eq, Ord, Show, Read, Semigroup)
  deriving anyclass (NFData)

instance Pretty SelectionSet where
  pretty (SelectionSet set) = vsep ["{", indent 2 (vsep (f <$> NE.toList set)), "}"]
    where
      f :: Selection -> Doc ann
      f = \case
        SelectionField field -> pretty field
        SelectionFragmentSpread fragmentSpread -> pretty fragmentSpread
        SelectionInlineFragment inlineFragment -> pretty inlineFragment

data Selection
  = SelectionField Field
  | SelectionFragmentSpread FragmentSpread
  | SelectionInlineFragment InlineFragment
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

--------------------------------------------------------------------------------
-- Values

newtype EnumValue = EnumValue {unEnum :: Text}
  deriving stock (Generic, Lift)
  deriving newtype (Eq, Ord, Show, Read)
  deriving anyclass (Hashable, NFData)

instance Pretty EnumValue where
  pretty = pretty . unEnum

data Value
  = VVar S.Span Text
  | VNull S.Span
  | VInt S.Span Integer
  | VFloat S.Span Scientific
  | VString S.Span Text
  | VBoolean S.Span Bool
  | VEnum S.Span EnumValue
  | VList S.Span [Value]
  | VObject S.Span (HashMap Name Value)
  deriving stock (Eq, Ord, Show, Read, Generic, Lift)
  deriving anyclass (Hashable, NFData)

instance S.Located Value where
  locate = \case
    VVar sp _txt -> sp
    VNull sp -> sp
    VInt sp _n -> sp
    VFloat sp _sci -> sp
    VString sp _txt -> sp
    VBoolean sp _b -> sp
    VEnum sp _ev -> sp
    VList sp _vas -> sp
    VObject sp _hm -> sp

instance Pretty Value where
  pretty = \case
    VVar _sp var -> "$" <> pretty var
    VNull _sp -> "null"
    VInt _sp n -> pretty n
    VFloat _sp sci -> pretty $ show sci
    VString _sp txt -> "\"" <> pretty txt <> "\""
    VBoolean _sp True -> "true"
    VBoolean _sp False -> "false"
    VEnum _sp ev -> pretty ev
    VList _sp xs -> pretty xs
    VObject _sp obj -> ob $ Map.toList obj
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

data OperationType
  = OperationTypeQuery
  | OperationTypeMutation
  | OperationTypeSubscription
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance Pretty OperationType where
  pretty = \case
    OperationTypeQuery -> "query"
    OperationTypeMutation -> "mutation"
    OperationTypeSubscription -> "subscription"

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

newtype VariablesDefinition = VariablesDefinition
  {variablesDefinition :: NE.NonEmpty VariableDefinition}
  deriving stock (Lift, Generic)
  deriving newtype (Eq, Ord, Show, Read, Semigroup)
  deriving anyclass (NFData)

instance Pretty VariablesDefinition where
  pretty (VariablesDefinition vars) =
    tupled (NE.toList $ fmap pretty vars)

data VariableDefinition = VariableDefinition
  { _vdSpan :: S.Span,
    _vdName :: Name,
    _vdType :: Type,
    _vdDefaultValue :: Maybe Value,
    _vdDirectives :: Maybe Directives
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance S.Located VariableDefinition where
  locate = _vdSpan

instance Pretty VariableDefinition where
  pretty VariableDefinition {..} =
    pretty _vdName <> ":" <+> pretty _vdType <-> fmap pretty _vdDefaultValue <-> fmap pretty _vdDirectives

data Arguments = Arguments
  { argSpan :: S.Span,
    argArguments :: HashMap Name Value
  }
  deriving stock (Eq, Ord, Show, Read, Generic, Lift)
  deriving anyclass (Hashable, NFData)

instance S.Located Arguments where
  locate = argSpan

instance Pretty Arguments where
  pretty Arguments {..} =
    if argArguments == mempty
      then mempty
      else tupled $ fmap (\(n, v) -> pretty n <> ":" <+> pretty v) (Map.toList argArguments)

newtype TypeCondition = TypeCondition {unTypeCondition :: Name}
  deriving stock (Generic, Lift)
  deriving stock (Eq, Ord, Show, Read)
  deriving anyclass (Hashable, NFData)

instance Pretty TypeCondition where
  pretty TypeCondition {..} =
    "on" <+> pretty unTypeCondition

data Type = NamedType S.Span Name | ListType S.Span Type | NonNullType S.Span Type
  deriving stock (Generic, Lift)
  deriving stock (Eq, Ord, Show, Read)
  deriving anyclass (Hashable, NFData)

instance S.Located Type where
  locate = \case
    NamedType sp _na -> sp
    ListType sp _ty -> sp
    NonNullType sp _ty -> sp

instance Pretty Type where
  pretty = \case
    NamedType _span ty -> pretty ty
    ListType _span xs -> "[" <> pretty xs <> "]"
    NonNullType _span ty -> pretty ty <> "!"

newtype Directives = Directives {unDirectives :: NE.NonEmpty Directive}
  deriving stock (Lift, Generic)
  deriving newtype (Eq, Ord, Show, Read, Semigroup)
  deriving anyclass (NFData)

instance Pretty Directives where
  pretty Directives {..} =
    fold $ punctuate " " $ fmap pretty $ NE.toList unDirectives

data Directive = Directive
  { _dSpan :: S.Span,
    _dName :: Name,
    _dArguments :: Maybe Arguments
  }
  deriving stock (Eq, Ord, Show, Read, Lift, Generic)
  deriving anyclass (NFData)

instance S.Located Directive where
  locate = _dSpan

instance Pretty Directive where
  pretty Directive {..} = "@" <> pretty _dName <> pretty _dArguments

--------------------------------------------------------------------------------
-- Pretty helpers

(<->) :: Doc a -> Maybe (Doc a) -> Doc a
(<->) x Nothing = x
(<->) x (Just y) = x <+> y

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
