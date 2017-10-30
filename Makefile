build:
	ocamlbuild -use-ocamlfind mlib.cma
	ocamlbuild -use-ocamlfind -Is src test/main.byte

clean:
	ocamlbuild -clean
