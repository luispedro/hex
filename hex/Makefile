all: hex hextest
.PHONY: all

hex: Main.lhs Chars.lhs
	ghc --make -O $^ -o $@

hextest: Tests.lhs Chars.lhs
	ghc --make -O $^ -o $@

