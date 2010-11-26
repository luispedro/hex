all: hex hextest
.PHONY: all

SOURCES := Chars.lhs Tokens.lhs

hex: Main.lhs $(SOURCES)
	ghc --make -O $^ -o $@

hextest: Tests.lhs $(SOURCES)
	ghc --make -O $^ -o $@

