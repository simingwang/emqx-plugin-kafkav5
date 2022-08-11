-module(rebar3_eetcd_plugin).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_eetcd_prv_compile:init(State),
    {ok, State1}.
