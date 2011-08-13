all: hex hextest dvidecode
.PHONY: all clean
GHCOPTS := -odir .objs -hidir .objs -Wall -fwarn-tabs -fno-warn-missing-signatures

SOURCES :=  \
            Boxes.lhs \
			Chars.lhs \
            CharStream.lhs \
            DVI.lhs \
            Environment.lhs \
            Fonts.lhs \
			Hex.lhs \
            Linebreak.lhs \
            LoadPL.lhs \
            Macros.lhs \
            Measures.lhs \
            Modes.lhs \
			PageBreak.lhs \
            Output.lhs \
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
