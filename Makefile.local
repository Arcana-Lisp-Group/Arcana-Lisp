test:: src/test/coq/verify.v
	for f in $^ ; do \
	  $(COQC) $(COQDEBUG) $(COQFLAGS) $(COQLIBS) $$f ; \
	done
