COMPILE=ghc -package wx

ifeq ($(shell uname),Darwin)
%: %.hs
	$(COMPILE) -o $@ $<
	macosx-app $@
else
%: %.hs
	$(COMPILE) -o $@ $<
endif

APPS= Hastur\

.PHONY: clean

all: $(APPS)

clean:
	rm -rf $(APPS) *.o *.hi *.app *exe *.manifest
