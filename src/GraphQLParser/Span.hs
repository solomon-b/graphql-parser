{-# LANGUAGE DeriveAnyClass #-}

module GraphQLParser.Span where

import Control.DeepSeq (NFData)
import Control.Lens (Lens', lens)
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)
import Prettyprinter (Pretty (..))

--------------------------------------------------------------------------------
-- Source Positions

data AlexSourcePos = AlexSourcePos {_line :: !Int, _col :: !Int}
  deriving stock (Show, Read, Eq, Ord, Lift, Generic)
  deriving anyclass (NFData)

col :: Lens' AlexSourcePos Int
col = lens _col (\pos col' -> pos {_col = col'})

line :: Lens' AlexSourcePos Int
line = lens _line (\pos line' -> pos {_line = line'})

alexStartPos :: AlexSourcePos
alexStartPos = AlexSourcePos 1 1

--------------------------------------------------------------------------------
-- Spans

data Span = Span {_start :: AlexSourcePos, _end :: AlexSourcePos}
  deriving stock (Show, Read, Eq, Ord, Lift, Generic)
  deriving anyclass (NFData)

instance Semigroup Span where
  (Span s1 e1) <> (Span s2 e2) = Span (min s1 s2) (max e1 e2)

start :: Lens' Span AlexSourcePos
start = lens _start (\pos start' -> pos {_start = start'})

end :: Lens' Span AlexSourcePos
end = lens _end (\pos end' -> pos {_end = end'})

--------------------------------------------------------------------------------
-- Locations

-- | The produce of @a@ and a 'Span' representing 'a's source position.
data Loc a = Loc {_span :: Span, unLoc :: a}
  deriving stock (Show, Eq, Ord, Functor, Generic)
  deriving anyclass (NFData)

instance Semigroup a => Semigroup (Loc a) where
  Loc s1 a1 <> Loc s2 a2 = Loc (s1 <> s2) (a1 <> a2)

instance Pretty a => Pretty (Loc a) where
  pretty (Loc _ a) = pretty a

span :: Lens' (Loc a) Span
span = lens _span (\loc span' -> loc {_span = span'})

-- | The class of types from which we can extract a 'Span'
class Located a where
  locate :: a -> Span

instance Located Span where
  {-# INLINE locate #-}
  locate x = x

instance Located (Loc a) where
  {-# INLINE locate #-}
  locate = _span

instance (Located a, Located b) => Located (Either a b) where
  {-# INLINE locate #-}
  locate = either locate locate
