all: erl.mk

erl.mk:
	wget -nv -O $@ 'https://raw.github.com/fenollp/erl-mk/master/erl.mk' || rm $@

DEPS = erlydtl
dep_erlydtl = git://github.com/erlydtl/erlydtl.git master

include erl.mk

# Your targets after this line.

distclean: clean clean-docs
	$(if $(wildcard deps/ ), rm -rf deps/)
	$(if $(wildcard erl.mk), rm erl.mk   )
	$(if $(wildcard erldocs), rm erldocs )
	$(if $(wildcard docs/), rm -rf docs/ )
.PHONY: distclean

all: escript

ebin/%_dtl.beam: templates/%.dtl                | ebin/
	$(if $(shell [[ ! -d deps/erlydtl ]] && echo y), \
	    $(error Error compiling $<: deps/erlydtl/ not found))
	@erl -noshell -pa ebin/ -pa deps/*/ebin/ \
	     -eval 'io:format("Compiling ErlyDTL template: $< -> $@\n").' \
	     -eval 'erlydtl:compile("$<", $*_dtl, [{out_dir,"ebin/"},{auto_escape,false}]).' \
	     -s init stop
