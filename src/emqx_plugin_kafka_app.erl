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
    FixedPath = "/etc/emqx_kafka.conf",
    CNF = 
        case filelib:is_file(FixedPath) of
            true ->
               %% {ok, Content} = file:read_file(FixedPath),
                {ok, Parsed} = hocon:load(FixedPath),
                logger:info("Config content ~p found, using it", [Parsed]),
                %% 现在可以使用原子键 kafka
                case maps:get(<<"kafka">>, Parsed, undefined) of
                    undefined ->
                        logger:error("Missing kafka section in config file ~s", [FixedPath]),
                        fallback_config();
                    KafkaConfig ->
                        case maps:get(<<"address_list">>, KafkaConfig, undefined) of
                            undefined ->
                                logger:error("Missing address_list in kafka config"),
                                fallback_config();
                            _ ->
                                Config = #{
                                    address_list =>  maps:get(<<"address_list">>, KafkaConfig, undefined),
                                    reconnect_cool_down_seconds => maps:get(<<"reconnect_cool_down_seconds">>, KafkaConfig, undefined),
                                    query_api_versions => maps:get(<<"query_api_versions">>, KafkaConfig, undefined),
                                    topic => maps:get(<<"topic">>, KafkaConfig, undefined),
                                    mqtt_topics => maps:get(<<"mqtt_topics">>, KafkaConfig, undefined)
                                },
                                application:set_env(emqx_plugin_kafka, kafka, Config),
                                Config
                        end    
                end;
            false ->
                logger:warning("Config file ~s not found, using default", [FixedPath]),
                fallback_config()
        end,
    CNF.

fallback_config() ->
    %% 修改后的环境变量获取方式
    AddressList = case os:getenv("KAFKA_ADDRESS_LIST") of
        false -> 
            %% 添加调试日志
            logger:debug("KAFKA_ADDRESS_LIST not found in env: ~p", [os:getenv()]),
            [];
        "" ->   %% 处理空字符串情况
            logger:warning("KAFKA_ADDRESS_LIST is empty"),
            [];
        Value -> Value
    end,
    logger:info("env address list : ~p", [AddressList]),
    
    Config = #{address_list => list_to_binary(AddressList),
      reconnect_cool_down_seconds => list_to_integer(
          case os:getenv("KAFKA_RECONNECT_COOL_DOWN_SECONDS") of
              false -> "10";
              Val -> Val
          end),
      query_api_versions => list_to_atom(
          case os:getenv("KAFKA_QUERY_API_VERSIONS") of
              false -> "true";
              Val -> Val
          end),
      topic => list_to_binary(case os:getenv("KAFKA_TOPIC") of
                false -> "mqtt-publish";
                T -> T
               end),
      mqtt_topics => 
          case os:getenv("KAFKA_MQTT_TOPICS") of
              false -> undefined;
              Topics -> list_to_binary(Topics)
          end
    },
    application:set_env(emqx_plugin_kafka, kafka, Config),
    Config.
