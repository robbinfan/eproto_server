all: compile

clean:
	./rebar clean

compile:
	./rebar compile

deps:
	./rebar get-deps

