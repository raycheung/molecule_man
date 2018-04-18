.DEFAULT_GOAL: all

all:
	ocamlbuild -pkg opium molecule_man.native

clean:
	rm -rf _build

.PHONY: all clean
