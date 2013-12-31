all: info html

info: web-server.texi
	makeinfo $<

html: web-server.texi
	makeinfo --html $<

clean:
	rm -f *.info;
	rm -rf web-server/
