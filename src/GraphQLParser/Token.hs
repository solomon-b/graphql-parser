{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}

module GraphQLParser.Token where

import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Generics (Generic)
import GraphQLParser.Span (Loc (..))
import Prettyprinter (Pretty (..))

--------------------------------------------------------------------------------

data Symbol
  = SymBang
  | SymBling
  | SymColon
  | SymComma
  | SymCurlyClose
  | SymCurlyOpen
  | SymDoubleQuote
  | SymParenClose
  | SymParenOpen
  | SymSquareClose
  | SymSquareOpen
  | SymSpread
  deriving (Show, Eq, Ord, Generic)

instance Pretty Symbol where
  pretty = \case
    SymBang -> "!"
    SymBling -> "$"
    SymColon -> ":"
    SymComma -> ","
    SymCurlyClose -> "}"
    SymCurlyOpen -> "{"
    SymDoubleQuote -> "\""
    SymParenClose -> ")"
    SymParenOpen -> "("
    SymSquareClose -> "]"
    SymSquareOpen -> "["
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
