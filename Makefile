all:
	ocamlbuild -I src argExt.cma argExt.cmxa

clean:
	ocamlbuild -clean
