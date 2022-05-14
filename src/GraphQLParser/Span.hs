module GraphQLParser.Span where

import Control.Lens (Lens', lens)

--------------------------------------------------------------------------------
-- Source Positions

data AlexSourcePos = AlexSourcePos {_line :: !Int, _col :: !Int}
  deriving (Show, Read, Eq, Ord)

col :: Lens' AlexSourcePos Int
col = lens _col (\pos col' -> pos {_col = col'})

line :: Lens' AlexSourcePos Int
line = lens _line (\pos line' -> pos {_line = line'})

alexStartPos :: AlexSourcePos
alexStartPos = AlexSourcePos 1 1

--------------------------------------------------------------------------------
-- Spans

data Span = Span {_start :: AlexSourcePos, _end :: AlexSourcePos}
  deriving (Show, Read, Eq, Ord)

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
  deriving (Show, Eq, Ord, Functor)

instance Semigroup a => Semigroup (Loc a) where
  Loc s1 a1 <> Loc s2 a2 = Loc (s1 <> s2) (a1 <> a2)

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
