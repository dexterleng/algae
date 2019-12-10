test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

check:
	bash checkenv.sh && bash checktypes.sh

clean:
	ocamlbuild -clean

run:
	ocamlbuild -use-ocamlfind project_compare.byte && ./project_compare.byte