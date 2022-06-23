{-# LANGUAGE DeriveAnyClass #-}

module GraphQLParser.Error
  ( -- * Parser Errors
    ParseError (..),
    parseError,

    -- * Error Serialization
    ErrorCode (..),
    SerializedError (..),
    serialize,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad.Except (MonadError (..))
import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import GraphQLParser.Span qualified as S
import GraphQLParser.Syntax (renderPretty)
import GraphQLParser.Token qualified as T
import Prettyprinter (Pretty (pretty), indent, vsep, (<+>))

--------------------------------------------------------------------------------

data ParseError
  = EmptyTokenStream S.Span BS.ByteString
  | UnexpectedToken T.Token BS.ByteString
  | InvalidLexeme S.AlexSourcePos BS.ByteString
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

instance Pretty ParseError where
  pretty = \case
    EmptyTokenStream sp source ->
      let S.AlexSourcePos {_col = startCol, _line = startLine} = S._start sp
          S.AlexSourcePos {_col = endCol} = S._end sp
       in mkPretty "Unexpected end of input" startCol startLine source (endCol - startCol)
    UnexpectedToken tok source ->
      let S.AlexSourcePos {_col = startCol, _line = startLine} = S._start $ S.locate tok
          S.AlexSourcePos {_col = endCol} = S._end $ S.locate tok
       in mkPretty "Unexpected token" startCol startLine source (endCol - startCol)
    InvalidLexeme S.AlexSourcePos {..} source -> mkPretty "Invalid Lexeme" _col _line source 1
    where
      mkPretty msg col' line' source len =
        let sourceLine = T.lines (TE.decodeUtf8 source) !! line'
         in vsep
              [ "Parse Error:",
                indent 2 msg,
                indent (length (show line') + 1) "|",
                pretty line' <+> "|" <+> pretty sourceLine,
                indent (length (show line') + 1) $ "|" <> indent col' (pretty (replicate len '^'))
              ]

parseError :: MonadError ParseError m => ParseError -> m a
parseError = throwError

--------------------------------------------------------------------------------

-- | The serialized representation of internal errors.
data SerializedError = SerializedError {_seCode :: ErrorCode, _seMessage :: T.Text, _seSpan :: S.Span}
  deriving (Show)

instance J.ToJSON SerializedError where
  toJSON (SerializedError ec msg span') =
    let (S.AlexSourcePos startLine startCol) = S._start span'
        (S.AlexSourcePos endLine endCol) = S._end span'
     in J.object
          [ "error_code" J..= J.String (renderPretty ec),
            "message" J..= J.String msg,
            "source_position"
              J..= J.object
                [ "start_line" J..= startLine,
                  "start_column" J..= startCol,
                  "end_line" J..= endLine,
                  "end_column" J..= endCol
                ]
          ]

data ErrorCode
  = EmptyTokenStreamCode
  | UnexpectedTokenCode
  | InvalidLexemeCode
  deriving (Show)

instance Pretty ErrorCode where
  pretty = \case
    EmptyTokenStreamCode -> "Empty Token Stream"
    UnexpectedTokenCode -> "Unexpected Token"
    InvalidLexemeCode -> "Invalid Lexeme"

--------------------------------------------------------------------------------

-- | Convert a 'ParserError' into the serialization type
-- 'SerializedError'. We distinguish these types to allow the internal
-- representation and serialation form to diverge structurally.
serialize :: ParseError -> SerializedError
serialize = \case
  EmptyTokenStream sp _src -> SerializedError {_seCode = EmptyTokenStreamCode, _seMessage = "Unexpected end of input.", _seSpan = sp}
  UnexpectedToken tok _src -> SerializedError {_seCode = EmptyTokenStreamCode, _seMessage = "Unexpecteed token.", _seSpan = S.locate tok}
  InvalidLexeme start src -> SerializedError {_seCode = EmptyTokenStreamCode, _seMessage = "Invalid Lexeme.", _seSpan = S.Span start (eofPos src)}

-- | Calculate the 'S.AlexSourcePos' of the EOF.
eofPos :: BS.ByteString -> S.AlexSourcePos
eofPos src = S.AlexSourcePos (length $ BS.split 10 src) (BS.length $ snd $ BS.spanEnd (/= 10) src)
