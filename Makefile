test:: src/test/coq/verify.v
	for f in $^ ; do \
	  $(COQC) $$f ; \
	done
