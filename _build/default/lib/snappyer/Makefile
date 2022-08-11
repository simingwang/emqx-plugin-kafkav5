.PHONY: clean compile tests hex-publish

all: compile

compile:
	@rebar3 compile -v

tests:
	@rebar3 eunit -v

clean:
	@rebar3 clean

hex-publish: clean
	rebar3 hex publish
