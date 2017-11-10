build:
	ocamlbuild -use-ocamlfind mlib.cma
	ocamlbuild -use-ocamlfind -Is lib -Is src test/samples.byte
	ocamlbuild -use-ocamlfind -Is lib -Is src test/docedit.byte
	ocamlbuild -use-ocamlfind -Is lib -Is src test/benchmarking.byte

clean:
	ocamlbuild -clean
