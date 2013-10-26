GHCFLAGS=-Wall -XNoCPP -fno-warn-name-shadowing -XHaskell98
HLINTFLAGS=-XHaskell98 -XNoCPP -i 'Use camelCase' -i 'Use String' -i 'Use string literal' -i 'Use list comprehension' --utf8 -XMultiParamTypeClasses
VERSION=0.2.0.0

.PHONY: all clean doc install

all: report.html doc dist/build/libHSplivo-$(VERSION).a dist/plivo-$(VERSION).tar.gz

install: dist/build/libHSplivo-$(VERSION).a
	cabal install

report.html: Plivo.hs
	-hlint $(HLINTFLAGS) --report Plivo.hs

doc: dist/doc/html/plivo/index.html README

README: plivo.cabal
	tail -n+$$(( `grep -n ^description: $^ | head -n1 | cut -d: -f1` + 1 )) $^ > .$@
	head -n+$$(( `grep -n ^$$ .$@ | head -n1 | cut -d: -f1` - 1 )) .$@ > $@
	-printf ',s/        //g\n,s/^.$$//g\nw\nq\n' | ed $@
	$(RM) .$@

dist/doc/html/plivo/index.html: dist/setup-config Plivo.hs
	cabal haddock --hyperlink-source

dist/setup-config: plivo.cabal
	cabal configure

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) report.html
	$(RM) -r dist dist-ghc

dist/build/libHSplivo-$(VERSION).a: plivo.cabal dist/setup-config Plivo.hs
	cabal build --ghc-options="$(GHCFLAGS)"

dist/plivo-$(VERSION).tar.gz: plivo.cabal dist/setup-config Plivo.hs README
	cabal check
	cabal sdist
