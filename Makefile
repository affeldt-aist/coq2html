PACKAGES=dune-glob
OCAMLOPT=ocamlfind ocamlopt -linkpkg -package $(PACKAGES) -I +str -I $(OPAM_SWITCH_PREFIX)/lib/dune-glob -annot
OCAMLLEX=ocamllex
OUTPUT=rocqnavi

GEN_IDX=generate_index

PROJ_OBJS=common.cmx glob_kind.cmx graphviz.cmx range.cmx xrefTable.cmx index_blacklist.cmx generate_index.cmx

all: $(OUTPUT) ocamldot/ocamldot

$(OUTPUT): $(PROJ_OBJS:.cmx=.cmi) $(PROJ_OBJS) $(OUTPUT).cmx
	$(OCAMLOPT) -o $(OUTPUT) str.cmxa resources.cmx $(PROJ_OBJS) $(OUTPUT).cmx


%.cmx: %.ml
	$(OCAMLOPT) -c $*.ml

%.cmi: %.mli
	$(OCAMLOPT) -c $*.mli

%.ml: %.mll
	$(OCAMLLEX) $*.mll

generate_index.cmx: resources.cmx

$(OUTPUT).cmx: resources.cmx
resources.cmx: resources.cmi

RESOURCES=header footer css js redirect

resources.ml: $(RESOURCES:%=$(OUTPUT).%)
	(for i in $(RESOURCES); do \
         echo "let $$i = {xxx|"; \
         cat $(OUTPUT).$$i; \
         echo '|xxx}'; \
         echo ''; \
         done) > resources.ml

.PHONY: test

test: $(OUTPUT)
	./test.sh

clean:
	rm -f $(OUTPUT)
	rm -f $(OUTPUT).ml resources.ml
	rm -f *.o *.cm?
	$(MAKE) -C ocamldot/ clean

PREFIX=/usr/local
BINDIR=$(PREFIX)/bin

install:
	install $(OUTPUT) $(BINDIR)/$(OUTPUT)

depend:
	ocamldep *.mli *.ml > .depend

-include .depend

# ocamldot
ocamldot/ocamldot: ocamldot/
	$(MAKE) -C ocamldot/ ocamldot
