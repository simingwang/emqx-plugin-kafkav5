%%--------------------------------------------------------------------
%% Copyright (c) 2020-2022 EMQ Technologies Co., Ltd. All Rights Reserved.
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
-module(emqx_plugin_kafka_schema).

-include_lib("hocon/include/hoconsc.hrl").
-include_lib("typerefl/include/types.hrl").

-behaviour(hocon_schema).

-export([
    namespace/0,
    roots/0,
    fields/1,
    desc/1
]).

namespace() -> kafka.

roots() -> [kafka].

fields(kafka) ->
    [
        {config_path,
            ?HOCON(
                string(),
                #{
                    required => false,
                    desc => ?DESC(config_path)
                }
            )},
        {address_list,
            ?HOCON(
                string(),
                #{
                    required => false,
                    desc => ?DESC(address_list)
                }
            )},
        {reconnect_cool_down_seconds,
            ?HOCON(
                emqx_schema:duration_ms(),
                #{
                    default => "10s",
                    required => false,
                    desc => ?DESC(reconnect_cool_down_seconds)
                }
            )},
        {query_api_versions,
            ?HOCON(
                boolean(),
                #{
                    default => true,
                    required => false,
                    desc => ?DESC(query_api_versions)
                }
            )},
        {mqtt_topics,
            ?HOCON(
                {array, string()},
                #{
                    required => false,
                    desc => ?DESC(mqtt_topics)
                }
            )},
        %% 保持向下兼容
        {topic,
            ?HOCON(
                string(),
                #{
                    required => false,
                    desc => ?DESC(topic)
                }
            )}
    ].

desc(kafka) -> "kafka plugin configuration.";
desc(_) -> undefined.
