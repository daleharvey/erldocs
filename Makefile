all: escript | erl.mk

erl.mk:
	curl -fsSLo $@ 'https://raw.github.com/fenollp/erl-mk/master/erl.mk' || rm $@

dep_erlydtl = git://github.com/erlydtl/erlydtl.git master

ERLCFLAGS += +debug_info

-include erl.mk
# Your targets after this line.
.PHONY: clean distclean test

clean: clean-ebin

distclean: clean clean-escript clean-deps
	$(if $(wildcard erl.mk), rm erl.mk   )
	$(if $(wildcard docs/), rm -rf docs/ )

test:
	./test/test.sh /tmp/erldocs.git
