# https://github.com/ocaml/ocamlbuild/blob/master/examples/05-lex-yacc/Makefile
#
#
# Pure OCaml, package from Opam, two directories
#

# - The -I flag introduces sub-directories
# - -use-ocamlfind is required to find packages (from Opam)
# - _tags file introduces packages, bin_annot flag for tool chain
# - using *.mll and *.mly are handled automatically

.PHONY: 	all clean byte native profile debug sanity test format

OCB_FLAGS   = -use-ocamlfind -use-menhir -I src -I src/codegen -I src/codegen_p -I lib -pkg ppx_deriving.show # uses menhir
OCB = ocamlbuild $(OCB_FLAGS)

all: native byte # profile debug

clean:
	$(OCB) -clean

native: sanity
	$(OCB) main.native

byte: sanity
	$(OCB) main.byte

profile: sanity
	$(OCB) -tag profile main.native

debug: sanity
	$(OCB) -tag debug main.byte

test: sanity
	$(OCB) test.native

format: sanity
	ocamlformat -i src/*.ml --enable-outside-detected-project

# check that menhir is installed, use "opam install menhir"
sanity:
	which menhir
