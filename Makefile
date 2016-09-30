DESTDIR=
PREFIX=/usr
BINDIR=/bin
CC?=cc
CFLAGS=-Wall -O2
OFLAGS=-O1
OWL=ol-0.1.13
OWLSHA=4dc2fe537f9d952d02e3c67564531c0466386b3d353a3
OWLURL=https://github.com/aoh/owl-lisp/files/449350
USR_BIN_OL=/usr/bin/ol

bin/opus: c/opus.c
	$(CC) $(CFLAGS) -O2 -o bin/opus c/opus.c 

c/opus.c: opus/*.scm bin/ol kal
	bin/ol $(OFLAGS) -o c/opus.c opus/main.scm

kal:
	mkdir -p ext
	cd ext && git clone https://github.com/aoh/kal.git
	ln -s ext/kal/kal

bin/ol:
	mkdir -p tmp c bin
	test -f tmp/$(OWL).c.gz || wget $(OWLURL)/$(OWL).c.gz
	mv $(OWL).c.gz tmp
	sha256sum tmp/$(OWL).c.gz | grep -q $(OWLSHA)
	gzip -d < tmp/$(OWL).c.gz > c/$(OWL).c
	cc $(CFLAGS) -o bin/ol c/$(OWL).c

sure:
	test/run opus/main.scm
