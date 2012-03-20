all: hex hextest dvidecode
.PHONY: all clean
GHCOPTS := -odir .objs -hidir .objs -Wall -fwarn-tabs -fno-warn-missing-signatures

SOURCES :=  \
            Boxes.lhs \
            Chars.lhs \
            CharStream.lhs \
            Defaults.lhs \
            DVI.lhs \
            Environment.lhs \
            FixWords.lhs \
            Fonts.lhs \
            Hex.lhs \
            Linebreak.lhs \
            Macros.lhs \
            Maths.lhs \
            Measures.lhs \
            Modes.lhs \
            Output.lhs \
            PageBreak.lhs \
			ParseTFM.lhs \
            String.lhs \
            Tokens.lhs

hex: Main.lhs $(SOURCES)
	ghc $(GHCOPTS) --make -O2 $^ -o $@

hextest: Tests.hs $(SOURCES)
	ghc $(GHCOPTS) --make -O $^ -o $@

dvidecode: DVIDecode.lhs DVI.lhs
	ghc $(GHCOPTS) --make -O2 $^ -o $@

clean:
	rm -rf .objs
	rm -f hex hextest dvidecode
