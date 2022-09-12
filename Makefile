.PHONY: format
format:
	cabal-fmt -i graphql-parser.cabal
	find src test \
	  -type f \( -name "*.hs" -o -name "*.hs-boot" \) | \
	  xargs ormolu -ie
