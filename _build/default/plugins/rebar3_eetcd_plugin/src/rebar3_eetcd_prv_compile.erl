-module(rebar3_eetcd_prv_compile).

-export([init/1, do/1, format_error/1]).

-include_lib("providers/include/providers.hrl").

-define(PROVIDER, gen).
-define(NAMESPACE, etcd).
-define(DEPS, [{default, app_discovery}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},            % The 'user friendly' name of the task
        {namespace, ?NAMESPACE},
        {module, ?MODULE},            % The module implementation of the task
        {bare, true},                 % The task can be run by the user, always true
        {deps, ?DEPS},                % The list of dependencies
        {example, "rebar3 etcd gen"}, % How to use the plugin
        {opts, []},                   % list of options understood by the plugin
        {short_desc, "Generates ETCD client protos"},
        {desc, "Generates ETCD V3 client protos"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Config = rebar_state:opts(State),
    EtcdConfig = rebar_opts:get(Config, etcd, []),
    {Options, _} = rebar_state:command_parsed_args(State),
    ProtoDir = proplists:get_value(protos, Options, proplists:get_value(protos, EtcdConfig, "priv/protos")),
    GpbOpts = proplists:get_value(gpb_opts, EtcdConfig, []),

    [begin
         GpbModule = compile_pb(Filename, GpbOpts),
         rebar_api:info("Compiled PB file ~s~n", [Filename]),
         gen_client_module(GpbModule, Options, EtcdConfig, State),
         rebar_api:info("Generated a client module for PB module ~s~n", [GpbModule])
     end || Filename <- filelib:wildcard(filename:join(ProtoDir, "*.proto"))],
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

compile_pb(Filename, Options) ->
    OutDir = proplists:get_value(o, Options, "src/protos"),
    ModuleNameSuffix = proplists:get_value(module_name_suffix, Options, "_pb"),
    ModuleNamePrefix = proplists:get_value(module_name_prefix, Options, ""),
    CompiledPB = filename:join(OutDir, ModuleNamePrefix ++ filename:basename(Filename, ".proto") ++ ModuleNameSuffix ++ ".erl"),
    rebar_api:info("Writing ~s", [CompiledPB]),
    GpbIncludeDir = filename:join(code:lib_dir(gpb), "include"),
    case compile:file(CompiledPB,
        [binary, {i, GpbIncludeDir}, {i, "./include/"}, return_errors]) of
        {ok, Module, Compiled} ->
            {module, _} = code:load_binary(Module, CompiledPB, Compiled),
            Module;
        {ok, Module, Compiled, Warnings} ->
            [begin
                 rebar_api:warn("Warning building ~s~n", [File]),
                 [rebar_api:warn("        ~p: ~s", [Line, M:format_error(E)]) || {Line, M, E} <- Es]
             end || {File, Es} <- Warnings],
            {module, _} = code:load_binary(Module, CompiledPB, Compiled),
            Module;
        {error, Errors, Warnings} ->
            throw(?PRV_ERROR({compile_errors, Errors, Warnings}))
    end.

gen_client_module(GpbModule, Options, EtcdConfig, State) ->
    OutDir = proplists:get_value(out_dir, EtcdConfig, "src"),
    Force = proplists:get_value(force, Options, true),
    Services = [format_services(GpbModule, Service, OutDir) || Service <- GpbModule:get_service_names()],
    rebar_api:debug("eetcd will run client module gen for these services: ~p", [Services]),
    [rebar_templater:new("eetcd_client", Service, Force, State) || Service <- Services].

format_services(GpbModule, Service, OutDir) ->
    begin
        {{_, NameAtom}, Methods} = GpbModule:get_service_def(Service),
        rebar_api:debug("eetcd service ~p has methods: ~p", [Service, Methods]),
        Name = atom_to_list(NameAtom),
        [_, Module] = string:tokens(Name, "."),

        [
            {out_dir,                 OutDir},
            {pb_module,               atom_to_list(GpbModule)},
            {unmodified_service_name, Name},
            {module_name,             list_snake_case(Module)},
            {methods,                 format_methods(Module, Name, GpbModule, Methods)}
        ]
    end.

format_methods(Module, _Name, GpbModule, Methods) ->
    [begin
        #{input         := Input,
          input_stream  := InputStream,
          name          := MethodName,
          output        := Output,
          output_stream := OutputStream} = Method,
         MethodNameStr = atom_to_list(MethodName),
         [
             {method,                  list_snake_case(MethodNameStr)},
             {unmodified_service_name, Module},
             {unmodified_method,       MethodNameStr},
             {full_service_path,       full_service_path(Module, MethodNameStr)},
             {pb_module,               atom_to_list(GpbModule)},
             {input,                   Input},
             {output,                  Output},
             {input_stream,            InputStream},
             {out_stream,              OutputStream}
         ]
     end || Method <- Methods].

list_snake_case(NameString) ->
    Snaked = lists:foldl(fun(RE, Snaking) ->
        re:replace(Snaking, RE, "\\1_\\2", [{return, list}, global])
                         end, NameString, [%% uppercase followed by lowercase
        "(.)([A-Z][a-z]+)",
        %% any consecutive digits
        "(.)([0-9]+)",
        %% uppercase with lowercase
        %% or digit before it
        "([a-z0-9])([A-Z])"]),
    Snaked1 = string:replace(Snaked, ".", "_", all),
    Snaked2 = string:replace(Snaked1, "__", "_", all),
    string:to_lower(unicode:characters_to_list(Snaked2)).

%% Calculates correct gRPC service path
full_service_path("Lock", Method)  ->
    io_lib:format("/v3lockpb.Lock/~s", [Method]);
full_service_path("Election", Method)  ->
    io_lib:format("/v3electionpb.Election/~s", [Method]);
full_service_path(Service, Method) ->
    io_lib:format("/etcdserverpb.~s/~s", [Service, Method]).
