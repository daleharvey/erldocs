all: compile
.PHONY: all

compile: rebar
	./rebar get-deps compile escriptize
.PHONY: compile

clean: rebar
	./rebar clean
.PHONY: clean

distclean: clean
	rm -rf ebin deps rebar erldocs
.PHONY: distclean

test: compile
	./rebar skip_deps=true eunit
.PHONY: test check

rebar:
	git clone git://github.com/rebar/rebar.git rebar.d
	cd rebar.d && ./bootstrap
	mv rebar.d/rebar $@
	rm -rf ./rebar.d
