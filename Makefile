build:
	ocamlbuild -use-ocamlfind mlib.cma
	ocamlbuild -use-ocamlfind -Is lib -Is src test/main.byte
	ocamlbuild -use-ocamlfind -Is lib -Is src test/docedit.byte

clean:
	ocamlbuild -clean
