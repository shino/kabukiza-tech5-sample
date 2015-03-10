.PHONY: all deps compile compile-top generate clean

all: deps compile generate

deps:
	./rebar get-deps

compile:
	./rebar compile

compile-top:
	./rebar compile skip_deps=true

generate:
	rm -rf rel/decho
	./rebar generate

stage: generate
	rm -rf rel/decho/lib/decho-1/*
	ln -s $(PWD)/ebin rel/decho/lib/decho-1/ebin 

clean:
	./rebar clean
