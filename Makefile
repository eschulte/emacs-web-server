EMACS := emacs
BATCH=$(EMACS) --batch --execute '(add-to-list (quote load-path) "$(shell pwd)")'

SRC=$(wildcard *.el)
ELC=$(SRC:.el=.elc)

.PHONY: clean src doc
all: src doc

src: $(SRC)
	$(BATCH) -f batch-byte-compile $^

doc:
	$(MAKE) -C doc/

clean:
	rm -f $(ELC)
	$(MAKE) -C doc/ $(MAKECMDGOALS)
