all: compile

clean:
	./rebar clean

compile:
	./rebar compile

deps:
	./rebar get-deps

rel:
	./rebar compile generate


REPO_TAG := $(shell git describe --tags)


