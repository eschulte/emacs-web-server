EMACS := emacs
BATCH=$(EMACS) --batch --execute '(add-to-list (quote load-path) "$(shell pwd)")'

SRC=$(wildcard *.el)
ELC=$(SRC:.el=.elc)

.PHONY: src doc check clean
all: src doc

src: $(SRC)
	$(BATCH) -f batch-byte-compile $^

doc:
	$(MAKE) -C doc/

check: $(SRC)
	$(BATCH) -l cl -l ert -l web-server-test -f ert-run-tests-batch-and-exit

clean:
	rm -f $(ELC)
	$(MAKE) -C doc/ $(MAKECMDGOALS)
