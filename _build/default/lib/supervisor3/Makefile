.PHONY: all
all:
	rebar3 do compile,dialyzer,eunit

.PHONY: hex-publish
hex-publish:
	$(verbose) rebar3 hex publish
