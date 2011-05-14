all: hex hextest dvidecode
.PHONY: all clean

SOURCES :=  \
            Boxes.lhs \
			Chars.lhs \
            DVI.lhs \
            Environment.lhs \
            Fonts.lhs \
            Linebreak.lhs \
            LoadPL.lhs \
            Macros.lhs \
            Modes.lhs \
			PageBreak.lhs \
            Output.lhs \
            String.lhs \
            Tokens.lhs

hex: Main.lhs $(SOURCES)
	ghc --make -O2 $^ -o $@

hextest: Tests.hs $(SOURCES)
	ghc --make -O $^ -o $@

dvidecode: DVIDecode.lhs DVI.lhs
	ghc --make -O2 $^ -o $@

clean:
	rm -f *.o *.lhi hex hextest
