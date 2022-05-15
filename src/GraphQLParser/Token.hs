{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}

module GraphQLParser.Token where

import Control.DeepSeq (NFData)
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import GHC.Generics
import GraphQLParser.Span (Loc (..))
import Prettyprinter (Doc, Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

--------------------------------------------------------------------------------

data Symbol
  = SymCurlyOpen
  | SymCurlyClose
  | SymParenOpen
  | SymParenClose
  | SymSquareOpen
  | SymSquareClose
  | SymColon
  | SymDoubleQuote
  | SymComma
  | SymSpread
  deriving (Show, Eq, Ord, Generic)

instance Pretty Symbol where
  pretty = \case
    SymCurlyOpen -> "{"
    SymCurlyClose -> "}"
    SymParenOpen -> "("
    SymParenClose -> ")"
    SymSquareOpen -> "["
    SymSquareClose -> "]"
    SymColon -> ":"
    SymDoubleQuote -> "\""
    SymComma -> ","
    SymSpread -> "..."

data Token
  = TokSymbol (Loc Symbol)
  | TokStringLit (Loc Text)
  | TokIdentifier (Loc Text)
  | TokDirective (Loc Text)
  | TokIntLit (Loc Integer)
  | TokNumLit Text (Loc Scientific)
  | TokBoolLit (Loc Bool)
  | EOF
  deriving (Show, Eq, Ord, Generic)

overLoc :: (forall a. Loc a -> Loc a) -> Token -> Token
overLoc f (TokSymbol loc) = TokSymbol $ f loc
overLoc f (TokStringLit loc) = TokStringLit $ f loc
overLoc f (TokDirective loc) = TokDirective $ f loc
overLoc f (TokIdentifier loc) = TokIdentifier $ f loc
overLoc f (TokIntLit loc) = TokIntLit $ f loc
overLoc f (TokNumLit txt loc) = TokNumLit txt $ f loc
overLoc f (TokBoolLit loc) = TokBoolLit $ f loc
overLoc _ EOF = EOF

instance Pretty Token where
  pretty = \case
    TokSymbol sym -> pretty sym
    TokStringLit str -> "\"" <> pretty str <> "\""
    TokDirective iden -> "@" <> pretty iden
    TokIdentifier iden -> pretty iden
    TokIntLit i -> pretty i
    TokNumLit txt _ -> pretty txt
    TokBoolLit b -> pretty b
    EOF -> mempty

--------------------------------------------------------------------------------
-- GraphQL IR

newtype Name = Name {unName :: Text}
  deriving newtype (Eq, Ord, Show, Read, Hashable, NFData)

newtype EnumValue = EnumValue {unEnum :: Text}
  deriving newtype (Eq, Ord, Show, Read)

data Directive = Directive
  { name :: Name,
    arguments :: HashMap Name Value
  }
  deriving stock (Eq, Ord, Show, Read)

-- TODO(Solomon): Description, Type
data Field = Field
  { fieldAlias :: Maybe Name,
    fieldName :: Name,
    fieldDirectives :: [Directive],
    fieldArguments :: HashMap Name Value,
    fieldSelectionSet :: SelectionSet
  }
  deriving stock (Eq, Ord, Show, Read)

newtype SelectionSet = SelectionSet [Selection]
  deriving newtype (Eq, Ord, Show, Read, Semigroup, Monoid)

-- TODO(Solomon): Fragment Spread and Fragment Inline
newtype Selection = SelField Field
  deriving newtype (Eq, Ord, Show, Read)

data Value
  = VNull
  | VInt Integer
  | VFloat Scientific
  | VString Text
  | VBoolean Bool
  | VEnum EnumValue
  | VList [Value]
  | VObject (HashMap Name Value)
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
