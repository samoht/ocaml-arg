all:
	ocamlbuild -I src arg.cma arg.cmxa

clean:
	ocamlbuild -clean
