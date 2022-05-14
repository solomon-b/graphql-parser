{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module GraphQLParser.Token where

import Data.ByteString.Lazy qualified as BL
import Data.Scientific (Scientific)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Data.Vector qualified as V
import GraphQLParser.Span (Loc (..))
import Prettyprinter (Doc, Pretty (..), defaultLayoutOptions, layoutPretty)

--------------------------------------------------------------------------------

data Symbol
  = SymCurlyOpen
  | SymCurlyClose
  | SymParenOpen
  | SymParenClose
  | SymColon
  deriving (Show, Eq, Ord)

instance Pretty Symbol where
  pretty = \case
    SymCurlyOpen -> "{"
    SymCurlyClose -> "}"
    SymParenOpen -> "("
    SymParenClose -> ")"
    SymColon -> ":"

data Token
  = TokSymbol (Loc Symbol)
  | TokStringLit (Loc T.Text)
  | TokIdentifier (Loc T.Text)
  | TokNumLit T.Text (Loc Scientific)
  | TokBoolLit (Loc Bool)
  | EOF
  deriving (Show, Eq, Ord)

overLoc :: (forall a. Loc a -> Loc a) -> Token -> Token
overLoc f (TokSymbol loc) = TokSymbol $ f loc
overLoc f (TokStringLit loc) = TokStringLit $ f loc
overLoc f (TokIdentifier loc) = TokIdentifier $ f loc
overLoc f (TokNumLit txt loc) = TokNumLit txt $ f loc
overLoc f (TokBoolLit loc) = TokBoolLit $ f loc
overLoc _ EOF = EOF

instance Pretty Token where
  pretty = \case
    TokSymbol sym -> pretty (unLoc sym)
    TokStringLit str -> "\"" <> pretty (unLoc str) <> "\""
    TokIdentifier iden -> pretty (unLoc iden)
    TokNumLit txt _ -> pretty txt
    TokBoolLit b -> pretty (unLoc b)
    EOF -> mempty

--------------------------------------------------------------------------------
-- GraphQL Syntax Tree

data GraphQL = GraphQL
  deriving (Show, Eq, Read)

--------------------------------------------------------------------------------
-- Pretty helpers

renderDoc :: Doc ann -> T.Text
renderDoc = renderStrict . layoutPretty defaultLayoutOptions

renderPretty :: Pretty a => a -> T.Text
renderPretty = renderDoc . pretty

renderVect :: Pretty a => V.Vector a -> T.Text
renderVect = renderDoc . foldMap pretty

renderBL :: BL.ByteString -> T.Text
renderBL = TE.decodeUtf8 . BL.toStrict
