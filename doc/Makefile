%.info: %.texi
	makeinfo $<

%.html: %.texi
	makeinfo --html $<

clean:
	rm -f *.info;
	rm -rf emacs-web-server/
