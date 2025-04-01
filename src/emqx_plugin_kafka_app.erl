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
    %case emqx_conf:get_raw([kafka]) of
    %     {config_not_found,[kafka]} ->
            #{
               address_list => os:getenv("KAFKA_ADDRESS_LIST") ,
               reconnect_cool_down_seconds => os:getenv("KAFKA_RECONNECT_COOL_DOWN_SECONDS") ,
               query_api_versions => os:getenv("KAFKA_QUERY_API_VERSIONS") ,
               topic => os:getenv("KAFKA_TOPIC") 
            }.
    %    _ ->
    %        emqx_conf:get_raw(kafka)
    %end.
