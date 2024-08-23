OCAMLOPT=ocamlopt -I +str
OCAMLLEX=ocamllex

GEN_IDX=generate_index

coq2html: $(GEN_IDX).cmi $(GEN_IDX).cmx coq2html.cmx
	$(OCAMLOPT) -o coq2html str.cmxa resources.cmx $(GEN_IDX).cmx coq2html.cmx

%.cmx: %.ml
	$(OCAMLOPT) -c $*.ml

%.cmi: %.mli
	$(OCAMLOPT) -c $*.mli

%.ml: %.mll
	$(OCAMLLEX) $*.mll


coq2html.cmx: resources.cmx
resources.cmx: resources.cmi

RESOURCES=header footer css redirect

resources.ml: $(RESOURCES:%=coq2html.%)
	(for i in $(RESOURCES); do \
         echo "let $$i = {xxx|"; \
         cat coq2html.$$i; \
         echo '|xxx}'; \
         echo ''; \
         done) > resources.ml

clean:
	rm -f coq2html
	rm -f coq2html.ml resources.ml
	rm -f *.o *.cm?

PREFIX=/usr/local
BINDIR=$(PREFIX)/bin

install:
	install coq2html $(BINDIR)/coq2html
