FROM ocaml/opam2:ubuntu

WORKDIR /usr/src/app

RUN sudo apt-get update -y
RUN sudo apt-get upgrade -y
RUN sudo apt-get install m4 -y

RUN opam init -a -y --disable-sandboxing
RUN opam update
RUN eval $(opam config env)
RUN opam install dune
RUN opam install core yojson ounit2 ppx_deriving_yojson

COPY . .

RUN opam install --deps-only .
RUN sudo opam config exec dune build ./project_compare.exe
