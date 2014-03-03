all: erl.mk

erl.mk:
	wget -nv -O $@ 'https://raw.github.com/fenollp/erl-mk/master/erl.mk' || rm $@

DEPS = erlydtl
dep_erlydtl = git://github.com/erlydtl/erlydtl.git 0.8.2

include erl.mk

# Your targets after this line.

distclean: clean clean-docs
	$(if $(wildcard deps/ ), rm -rf deps/)
	$(if $(wildcard erl.mk), rm erl.mk   )
	$(if $(wildcard erldocs), rm erldocs )
	$(if $(wildcard docs/), rm -rf docs/ )
.PHONY: distclean

all: escript
