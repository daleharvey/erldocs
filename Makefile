.PHONY: deps

all:
	./rebar compile escriptize

deps:
	./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@rm -rf erldocs deps
