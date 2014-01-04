.PHONY: all
	
all:
	ocamlbuild -use-ocamlfind -classic-display main.native

clean:
	ocamlbuild -clean

test: all
	./main.native
