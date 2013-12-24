all: info html

info: emacs-web-server.texi
	makeinfo $<

html: emacs-web-server.texi
	makeinfo --html $<

clean:
	rm -f *.info;
	rm -rf emacs-web-server/
