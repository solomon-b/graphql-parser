[![graphql-parser::CI](https://github.com/solomon-b/graphql-parser/actions/workflows/haskell.yml/badge.svg)](https://github.com/solomon-b/graphql-parser/actions/workflows/haskell.yml)

# GraphQL Parser

This parser is written using Happy and Alex and aim to be a feature
complete to the current [GraphQL Spec](https://spec.graphql.org/).

```
> :t runParseGraphQL 
runParseGraphQL :: B.ByteString -> Either ParseError Document

> runParseGraphQL "type Foo { bar: Int }"

Right [DefinitionTypeSystem (TypeSystemDefinitionType (OTDef (ObjectTypeDefinition {_otdDescription = Nothing, _otdName = "Foo", _otdImplementsInterfaces = Nothing, _otdDirectives = Nothing, _otdFieldsDefinition = Just (FieldDefinition {_fldDescription = Nothing, _fldName = "bar", _fldArgumentsDefinition = [], _fldType = NamedType "Int", _fldDirectives = Nothing} :| [])})))]
```

Parse Errors are helpful and can be pretty printed or serialized:
```
> runParseGraphQL "type Foo  bar: Int }"
Left (UnexpectedToken (TokIdentifier (Loc {_span = Span {_start = AlexSourcePos {_line = 0, _col = 11}, _end = AlexSourcePos {_line = 0, _col = 14}}, unLoc = "bar"})) "type Foo  bar: Int }")

> first (toJSON . serialize) $ runParseGraphQL "type Foo  bar: Int }"
Left (Object (fromList [("error_code",String "Empty Token Stream"),("message",String "Unexpecteed token."),("source_position",Object (fromList [("end_column",Number 14.0),("end_line",Number 0.0),("start_column",Number 11.0),("start_line",Number 0.0)]))]))

> :t err

err :: ParseError
> putStrLn  $ unpack $ renderPretty err
Parse Error:
  Unexpected token
  |
0 | type Foo  bar: Int }
  |           ^^^
```

QuasiQuoters are provided to construct literal values at compile time:
```
> [documentQQ|mutation {likeStory(storyID: 12345) {story {likeCount}}}|]

[DefinitionExecutable (ExecutableDefinitionOperation (OperationDefinition {_odType = OperationTypeMutation, _odName = Nothing, _odVariables = Nothing, _odDirectives = Nothing, _odSelectionSet = SelectionField (Field {_fAlias = Nothing, _fName = "likeStory", _fArguments = fromList [("storyID",VInt 12345)], _fDirectives = Nothing, _fSelectionSet = Just (SelectionField (Field {_fAlias = Nothing, _fName = "story", _fArguments = fromList [], _fDirectives = Nothing, _fSelectionSet = Just (SelectionField (Field {_fAlias = Nothing, _fName = "likeCount", _fArguments = fromList [], _fDirectives = Nothing, _fSelectionSet = Nothing}) :| [])}) :| [])}) :| []}))]
```

# TODO

- Review and clean up parser grammar
- Type System Extensions
- Improve Haddocks
