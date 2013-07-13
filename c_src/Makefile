### Makefile ---

## Author: frank@Hina.local
## Version: $Id: Makefile,v 0.0 2013/06/14 16:08:19 frank Exp $
## Keywords:
## X-URL:

librsync:
	cd ../librsync && make

internal: ../librsync/.libs/librsync.a internal.c internal.h
	cc -c -I ../librsync internal.c -o internal.o

all: internal
	cp ../librsync/.libs/librsync.a libhlibrsync.a
	ar -rcs libhlibrsync.a internal.o

test: all
	rm internal.o
	gcc -I ../librsync test.c libhlibrsync.a -o test

clean: 
	rm -f libhlibrsync.a internal.o

### Makefile ends here
