.PHONY: all clean compile tests hex-publish

all: compile

compile:
	@rebar3 compile

tests:
	@rebar3 eunit -v

clean:
	@rebar3 clean

hex-publish: clean
	@rebar3 hex publish
