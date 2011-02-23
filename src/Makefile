#!/usr/bin/make -f
#Template Makefile especially suited for Erlang directories

SHELL = /bin/sh
INITMOD = urlang
INITFUNC = init
#INITARGS = []

# Assume GNU
SED = sed -r
FIND = find . -regextype posix-extended


all:
	$(FIND) -name "*.erl" -exec erlc '{}' \;

clean:
	$(FIND)  \( -regex "^[.]?(.+)\~$$" -o -regex "./[.]?#.*#" \) -delete
	$(FIND)  -name "*.beam" -delete

distclean: clean
	$(FIND)  -name *.dump -delete

run:
	erl -noshell -s $(INITMOD) $(INITFUNC) -pa $(CURDIR)

run2:
	erl -s $(INITMOD) $(INITFUNC) -pa $(CURDIR)

.PHONY : clean distclean all run
