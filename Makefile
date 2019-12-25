test:
	corebuild -use-ocamlfind test.byte && ./test.byte

check:
	bash checkenv.sh && bash checktypes.sh

clean:
	corebuild-clean

run:
	corebuild -use-ocamlfind project_compare.byte && ./project_compare.byte
