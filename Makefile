all: erl.mk | ensure

erl.mk:
	curl -fsSLo $@ 'https://raw.github.com/fenollp/erl-mk/master/erl.mk' || rm $@

dep_erlydtl = git://github.com/erlydtl/erlydtl.git master

include erl.mk

# Your targets after this line.

distclean: clean clean-docs
	$(if $(wildcard deps/ ), rm -rf deps/)
	$(if $(wildcard erl.mk), rm erl.mk   )
	$(if $(wildcard erldocs), rm erldocs )
	$(if $(wildcard docs/), rm -rf docs/ )
.PHONY: distclean

ensure:
	test -f $(APP) && rm $(APP) || true

all: escript
