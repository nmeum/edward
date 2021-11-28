#!/bin/sh
/usr/bin/csc -setup-mode -host -I /home/soeren/src/edward -C \
	-I/home/soeren/src/edward -R r7rs -profile \
	"$@" edward.scm -o /home/soeren/src/edward/edward
