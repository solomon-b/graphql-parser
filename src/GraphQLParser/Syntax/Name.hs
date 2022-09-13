module GraphQLParser.Syntax.Name
  ( Name,
    unName,
    mkName,
    unsafeMkName,
  )
where

--------------------------------------------------------------------------------

import Control.DeepSeq (NFData)
import Data.Char qualified as Char
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text qualified as Text
import Language.Haskell.TH.Syntax (Lift)
import Prettyprinter (Pretty (..))

--------------------------------------------------------------------------------

newtype Name = Name {unName :: Text}
  deriving stock (Lift)
  deriving newtype (Eq, Ord, Show, Read, Hashable, NFData, Pretty)

mkName :: Text -> Maybe Name
mkName text =
  Text.uncons text >>= \(first, body) ->
    if matchFirst first && Text.all matchBody body
      then Just (Name text)
      else Nothing
  where
    matchFirst c = c == '_' || Char.isAsciiUpper c || Char.isAsciiLower c
    matchBody c = c == '_' || Char.isAsciiUpper c || Char.isAsciiLower c || Char.isDigit c

unsafeMkName :: Text -> Name
unsafeMkName = Name
