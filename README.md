[![graphql-parser::CI](https://github.com/solomon-b/graphql-parser/actions/workflows/haskell.yml/badge.svg)](https://github.com/solomon-b/graphql-parser/actions/workflows/haskell.yml)

# GraphQL Parser

This parser is written using Happy and Alex and aim to be a feature
complete to the current [GraphQL Spec](https://spec.graphql.org/).

Parsing functions are provided for Exectuable Documents, Type System Documents, and mixed GraphQL Documents:
```
> :t runParseExecutable 
runParseExecutable :: B.ByteString -> Either ParseError ExecutableDocument
> :t runParseTypeSystem 
runParseTypeSystem :: B.ByteString -> Either ParseError TypeSystemDocument
> :t runParseGraphQL 
runParseGraphQL :: B.ByteString -> Either ParseError GraphQLDocument
```

```
> runParseTypeSystem "type Foo { bar: Int }"
Right [Left (Left (Right (Left (Left (Left (Left (Right (ObjectTypeDefinition {objectDescription = Nothing, objectName = "Foo", objectInterfaces = [], objectDirectives = [], objectFields = [FieldDefinition {fieldDefDescription = Nothing, fieldDefName = "bar", fieldDefArgumentsDef = [], fieldDefType = NamedType "Int", fieldDefDirectives = []}]}))))))))]

> runParseExecutable  "type Foo { bar: Int }"
Left (UnexpectedToken (Loc {_span = Span {_start = AlexSourcePos {_line = 0, _col = 21}, _end = AlexSourcePos {_line = 0, _col = 22}}, unLoc = TokIdentifier (Loc {_span = Span {_start = AlexSourcePos {_line = 0, _col = 1}, _end = AlexSourcePos {_line = 0, _col = 5}}, unLoc = "type"})}) "type Foo { bar: Int }")
```

QuasiQuoters are provided to construct literal values at compile time:
```
> [executableDocQQ|mutation {likeStory(storyID: 12345) {story {likeCount}}}|]

[Left (OperationDefinition {opType = Mutation, opName = Nothing, opVariables = [], opDirectives = [], opSelectionSet = Left (Left (Field {fieldAlias = Nothing, fieldName = "likeStory", fieldArguments = fromList [("storyID",VInt 12345)], fieldDirectives = [], fieldSelectionSet = Just (Left (Left (Field {fieldAlias = Nothing, fieldName = "story", fieldArguments = fromList [], fieldDirectives = [], fieldSelectionSet = Just (Left (Left (Field {fieldAlias = Nothing, fieldName = "likeCount", fieldArguments = fromList [], fieldDirectives = [], fieldSelectionSet = Nothing})) :| [])})) :| [])})) :| []})]
```

# TODO

- Review and clean up prettty printer output
- Review and clean up parser grammar
- Type System Extensions
- Improve Haddocks
