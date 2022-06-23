{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}

module GraphQLParser.Token where

import Control.DeepSeq (NFData)
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Generics (Generic)
import GraphQLParser.Span (Loc (..), Located (..), Span)
import Prettyprinter (Pretty (..))

--------------------------------------------------------------------------------

data Symbol
  = SymAmpersand
  | SymAt
  | SymBang
  | SymBling
  | SymBlockQuote
  | SymColon
  | SymCurlyClose
  | SymCurlyOpen
  | SymDoubleQuote
  | SymEq
  | SymParenClose
  | SymParenOpen
  | SymPipe
  | SymSquareClose
  | SymSquareOpen
  | SymSpread
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Pretty Symbol where
  pretty = \case
    SymAmpersand -> "&"
    SymAt -> "@"
    SymBang -> "!"
    SymBling -> "$"
    SymBlockQuote -> "\"\"\""
    SymColon -> ":"
    SymCurlyClose -> "}"
    SymCurlyOpen -> "{"
    SymDoubleQuote -> "\""
    SymEq -> "="
    SymParenClose -> ")"
    SymParenOpen -> "("
    SymPipe -> "|"
    SymSquareClose -> "]"
    SymSquareOpen -> "["
    SymSpread -> "..."

data Token
  = TokSymbol (Loc Symbol)
  | TokStringBlock (Loc Text)
  | TokStringLit (Loc Text)
  | TokIdentifier (Loc Text)
  | TokDirective (Loc Text)
  | TokIntLit (Loc Integer)
  | TokNumLit Text (Loc Scientific)
  | TokBoolLit (Loc Bool)
  | EOF Span
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Located Token where
  locate = \case
    TokSymbol sym -> locate sym
    TokStringBlock str -> locate str
    TokStringLit str -> locate str
    TokIdentifier iden -> locate iden
    TokDirective dir -> locate dir
    TokIntLit int -> locate int
    TokNumLit _ loc -> locate loc
    TokBoolLit bool -> locate bool
    EOF sp -> sp

overLoc :: (forall a. Loc a -> Loc a) -> Token -> Token
overLoc f (TokSymbol loc) = TokSymbol $ f loc
overLoc f (TokStringBlock loc) = TokStringBlock $ f loc
overLoc f (TokStringLit loc) = TokStringLit $ f loc
overLoc f (TokDirective loc) = TokDirective $ f loc
overLoc f (TokIdentifier loc) = TokIdentifier $ f loc
overLoc f (TokIntLit loc) = TokIntLit $ f loc
overLoc f (TokNumLit txt loc) = TokNumLit txt $ f loc
overLoc f (TokBoolLit loc) = TokBoolLit $ f loc
overLoc _ (EOF sp) = EOF sp

instance Pretty Token where
  pretty = \case
    TokSymbol sym -> pretty sym
    TokStringBlock str -> "\"\"\"" <> pretty str <> "\"\"\""
    TokStringLit str -> "\"" <> pretty str <> "\""
    TokDirective iden -> "@" <> pretty iden
    TokIdentifier iden -> pretty iden
    TokIntLit i -> pretty i
    TokNumLit txt _ -> pretty txt
    TokBoolLit b -> pretty b
    EOF _ -> mempty
