%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emqx_plugin_kafka_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-export([ start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqx_plugin_kafka_sup:start_link(),
    Cnf = get_kafka_config(),
    emqx_plugin_kafka:load(Cnf),
    {ok, Sup}.
stop(_State) ->
    emqx_plugin_kafka:unload().

get_kafka_config() ->
    case maps:find(config_path, application:get_env(emqx_plugin_kafka, kafka, #{})) of
        {ok, Path} ->
            case filelib:is_file(Path) of
                true ->
                    try
                        {ok, Content} = file:read_file(Path),
                        {ok, Parssed} = hocon:binary(Content, #{format => map}),
                        case maps:get(kafka, Parssed, undefined) of
                                undefined ->
                                    logger:error("Missing kafka section in config file ~s", [Path]),
                                    fallback_config();
                                KafkaConfig ->
                                    case maps:get(address_list, KafkaConfig, undefined) of
                                        undefined ->
                                            logger:error("Missing address_list in kafka config"),
                                            fallback_config();
                                        _ ->
                                            KafkaConfig
                                    end
                        end
                    catch
                        _:Error:Reason ->
                            logger:error("Failed to parse config file ~s: ~p", [Path, {Error, Reason}]),
                            fallback_config()
                    end;
                false ->
                    logger:warning("Config file ~s not found, using default", [Path]),
                    fallback_config()
            end;
        _ ->
            fallback_config()
    end.

fallback_config() ->
    #{address_list => string:tokens(os:getenv("KAFKA_ADDRESS_LIST", ""), ","),
      reconnect_cool_down_seconds => list_to_integer(os:getenv("KAFKA_RECONNECT_COOL_DOWN_SECONDS", "10")),
      query_api_versions => list_to_atom(os:getenv("KAFKA_QUERY_API_VERSIONS", "true")),
      topic => os:getenv("KAFKA_TOPIC", "mqtt-publish")}.
