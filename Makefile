all:
	raco jeremiah make -s site.rkt

verbose:
	PLTSTDERR="info@jeremiah error" raco jeremiah make -s site.rkt

draft:
	PLTSTDERR="info@jeremiah error" raco jeremiah make -s site.rkt --include-draft

clean:
	rm -rf cache build/* # should not delete .git
