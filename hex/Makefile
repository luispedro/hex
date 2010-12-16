all: hex hextest
.PHONY: all

SOURCES :=  Chars.lhs \
            Tokens.lhs \
            Macros.lhs \
            String.lhs \
            LoadPL.lhs \
            DVI.lhs \
            Linebreak.lhs \
            Boxes.lhs \
            Output.lhs \
            Fonts.lhs

hex: Main.lhs $(SOURCES)
	ghc --make -O2 $^ -o $@

hextest: Tests.lhs $(SOURCES)
	ghc --make -O $^ -o $@

