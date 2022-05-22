[![graphql-parser::CI](https://github.com/solomon-b/graphql-parser/actions/workflows/haskell.yml/badge.svg)](https://github.com/solomon-b/graphql-parser/actions/workflows/haskell.yml)

# GraphQL Parser

This parser is written using Happy and Alex and aim to be a feature
complete to the current [GraphQL Spec](https://spec.graphql.org/).

```
> [executableDocQQ|mutation {likeStory(storyID: 12345) {story {likeCount}}}|]

[Left (OperationDefinition {opType = Mutation, opName = Nothing, opVariables = [], opDirectives = [], opSelectionSet = Left (Left (Field {fieldAlias = Nothing, fieldName = "likeStory", fieldArguments = fromList [("storyID",VInt 12345)], fieldDirectives = [], fieldSelectionSet = Just (Left (Left (Field {fieldAlias = Nothing, fieldName = "story", fieldArguments = fromList [], fieldDirectives = [], fieldSelectionSet = Just (Left (Left (Field {fieldAlias = Nothing, fieldName = "likeCount", fieldArguments = fromList [], fieldDirectives = [], fieldSelectionSet = Nothing})) :| [])})) :| [])})) :| []})]
```
