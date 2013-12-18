EMACS := emacs
BATCH=$(EMACS) --batch --execute '(add-to-list (quote load-path) "$(shell pwd)")'

SRC=$(wildcard *.el)
ELC=$(SRC:.el=.elc)

all: src

src: $(SRC)
	$(BATCH) -f batch-byte-compile $^

clean:
	rm -f $(ELC)
