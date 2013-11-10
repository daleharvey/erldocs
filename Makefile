all: compile
.PHONY: all

compile: rebar
	./rebar get-deps compile
.PHONY: compile

clean: rebar
	./rebar clean
.PHONY: clean

distclean: clean
	rm -rf ebin/ deps/ erldocs
.PHONY: distclean

rebar:
	git clone git://github.com/rebar/rebar.git rebar.d
	cd rebar.d && ./bootstrap
	mv rebar.d/rebar $@
	rm -rf ./rebar.d
