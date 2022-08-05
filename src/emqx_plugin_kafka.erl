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

-module(emqx_plugin_kafka).

%% for #message{} record
%% no need for this include if we call emqx_message:to_map/1 to convert it to a map
-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/emqx_hooks.hrl").

%% for logging
-include_lib("emqx/include/logger.hrl").

-export([ load/1
        , unload/0
        ]).

%% Client Lifecircle Hooks
-export([ on_client_connect/3
        , on_client_connack/4
        , on_client_connected/3
        , on_client_disconnected/4
        , on_client_authenticate/3
        , on_client_authorize/5
        , on_client_subscribe/4
        , on_client_unsubscribe/4
        ]).

%% Session Lifecircle Hooks
-export([ on_session_created/3
        , on_session_subscribed/4
        , on_session_unsubscribed/4
        , on_session_resumed/3
        , on_session_discarded/3
        , on_session_takeovered/3
        , on_session_terminated/4
        ]).

%% Message Pubsub Hooks
-export([ on_message_publish/2
        , on_message_delivered/3
        , on_message_acked/3
        , on_message_dropped/4
        ]).


%% Called when the plugin application start
load(Env) ->
  kafka_init([Env]),
  emqx:hook('client.connect', {?MODULE, on_client_connect, [Env]}),
  emqx:hook('client.connack', {?MODULE, on_client_connack, [Env]}),
  emqx:hook('client.connected', {?MODULE, on_client_connected, [Env]}),
  emqx:hook('client.disconnected', {?MODULE, on_client_disconnected, [Env]}),
  emqx:hook('client.authenticate', {?MODULE, on_client_authenticate, [Env]}),
  emqx:hook('client.check_acl', {?MODULE, on_client_check_acl, [Env]}),
  emqx:hook('client.subscribe', {?MODULE, on_client_subscribe, [Env]}),
  emqx:hook('client.unsubscribe', {?MODULE, on_client_unsubscribe, [Env]}),
  emqx:hook('session.created', {?MODULE, on_session_created, [Env]}),
  emqx:hook('session.subscribed', {?MODULE, on_session_subscribed, [Env]}),
  emqx:hook('session.unsubscribed', {?MODULE, on_session_unsubscribed, [Env]}),
  emqx:hook('session.resumed', {?MODULE, on_session_resumed, [Env]}),
  emqx:hook('session.discarded', {?MODULE, on_session_discarded, [Env]}),
  emqx:hook('session.takeovered', {?MODULE, on_session_takeovered, [Env]}),
  emqx:hook('session.terminated', {?MODULE, on_session_terminated, [Env]}),
  emqx:hook('message.publish', {?MODULE, on_message_publish, [Env]}),
  emqx:hook('message.delivered', {?MODULE, on_message_delivered, [Env]}),
  emqx:hook('message.acked', {?MODULE, on_message_acked, [Env]}),
  emqx:hook('message.dropped', {?MODULE, on_message_dropped, [Env]}).

on_client_connect(ConnInfo = #{clientid := ClientId}, Props, _Env) ->
  %% ?LOG_INFO("[KAFKA PLUGIN]Client(~s) connect, ConnInfo: ~p, Props: ~p~n",  [ClientId, ConnInfo, Props]),
  ok.

on_client_connack(ConnInfo = #{clientid := ClientId}, Rc, Props, _Env) ->
  %% ?LOG_INFO("[KAFKA PLUGIN]Client(~s) connack, ConnInfo: ~p, Rc: ~p, Props: ~p~n", [ClientId, ConnInfo, Rc, Props]),
  ok.

on_client_connected(ClientInfo = #{clientid := ClientId}, ConnInfo, _Env) ->
  %% ?LOG_INFO("[KAFKA PLUGIN]Client(~s) connected, ClientInfo:~n~p~n, ConnInfo:~n~p~n",[ClientId, ClientInfo, ConnInfo]),
  {IpAddr, _Port} = maps:get(peername, ConnInfo),
  Action = <<"connected">>,
  Now = now_mill_secs(os:timestamp()),
  Online = 1,
  Payload = [
    {action, Action},
    {device_id, ClientId},
    {username, maps:get(username, ClientInfo)},
    {keepalive, maps:get(keepalive, ConnInfo)},
    {ipaddress, iolist_to_binary(ntoa(IpAddr))},
    {proto_name, maps:get(proto_name, ConnInfo)},
    {proto_ver, maps:get(proto_ver, ConnInfo)},
    {ts, Now},
    {online, Online}
  ],
  produce_kafka_payload(ClientId, Payload),
  ok.

on_client_disconnected(ClientInfo = #{clientid := ClientId}, ReasonCode, ConnInfo, _Env) ->
  %% ?LOG_INFO("[KAFKA PLUGIN]Client(~s) disconnected due to ~p, ClientInfo:~n~p~n, ConnInfo:~n~p~n", [ClientId, ReasonCode, ClientInfo, ConnInfo]),
  Action = <<"disconnected">>,
  Now = now_mill_secs(os:timestamp()),
  Online = 0,
  Payload = [
    {action, Action},
    {device_id, ClientId},
    {username, maps:get(username, ClientInfo)},
    {reason, ReasonCode},
    {ts, Now},
    {online, Online}
  ],
  produce_kafka_payload(ClientId, Payload),
  ok.

on_client_authenticate(_ClientInfo = #{clientid := ClientId}, Result, _Env) ->
 %%  ?LOG_INFO("[KAFKA PLUGIN]Client(~s) authenticate, Result:~n~p~n", [ClientId, Result]),
  ok.

on_client_check_acl(_ClientInfo = #{clientid := ClientId}, Topic, PubSub, Result, _Env) ->
 %%  ?LOG_INFO("[KAFKA PLUGIN]Client(~s) check_acl, PubSub:~p, Topic:~p, Result:~p~n",[ClientId, PubSub, Topic, Result]),
  ok.

%%---------------------------client subscribe start--------------------------%%
on_client_subscribe(#{clientid := ClientId}, _Properties, TopicFilters, _Env) ->
  %% ?LOG_INFO("[KAFKA PLUGIN]Client(~s) will subscribe: ~p~n", [ClientId, TopicFilters]),
  Topic = erlang:element(1, erlang:hd(TopicFilters)),
  Qos = erlang:element(2, lists:last(TopicFilters)),
  Action = <<"subscribe">>,
  Now = now_mill_secs(os:timestamp()),
  Payload = [
    {device_id, ClientId},
    {action, Action},
    {topic, Topic},
    {qos, maps:get(qos, Qos)},
    {ts, Now}
  ],
  produce_kafka_payload(ClientId, Payload),
  ok.
%%---------------------client subscribe stop----------------------%%
on_client_unsubscribe(#{clientid := ClientId}, _Properties, TopicFilters, _Env) ->
  %% ?LOG_INFO("[KAFKA PLUGIN]Client(~s) will unsubscribe ~p~n", [ClientId, TopicFilters]),
  Topic = erlang:element(1, erlang:hd(TopicFilters)),
  Action = <<"unsubscribe">>,
  Now = now_mill_secs(os:timestamp()),
  Payload = [
    {device_id, ClientId},
    {action, Action},
    {topic, Topic},
    {ts, Now}
  ],
  produce_kafka_payload(ClientId, Payload),
  ok.

on_message_dropped(#message{topic = <<"$SYS/", _/binary>>}, _By, _Reason, _Env) ->
  ok;
on_message_dropped(Message, _By = #{node := Node}, Reason, _Env) ->
  %% ?LOG_INFO("[KAFKA PLUGIN]Message dropped by node ~s due to ~s: ~s~n",  [Node, Reason, emqx_message:format(Message)]),
  ok.


%%---------------------------message publish start--------------------------%%
on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}, _Env) ->
  ok;
on_message_publish(Message, _Env) ->
  {ok, ClientId, Payload} = format_payload(Message),
  produce_kafka_payload(ClientId, Payload),
  ok.
%%---------------------message publish stop----------------------%%

on_message_delivered(_ClientInfo = #{clientid := ClientId}, Message, _Env) ->
  %% ?LOG_INFO("[KAFKA PLUGIN]Message delivered to client(~s): ~s~n", [ClientId, emqx_message:format(Message)]),
  Topic = Message#message.topic,
  Payload = Message#message.payload,
  Qos = Message#message.qos,
  From = Message#message.from,
  Timestamp = Message#message.timestamp,
  Content = [
    {action, <<"message_delivered">>},
    {from, From},
    {to, ClientId},
    {topic, Topic},
    {payload, Payload},
    {qos, Qos},
    {cluster_node, node()},
    {ts, Timestamp}
  ],
  produce_kafka_payload(ClientId, Content),
  ok.

on_message_acked(_ClientInfo = #{clientid := ClientId}, Message, _Env) ->
  %% ?LOG_INFO("[KAFKA PLUGIN]Message acked by client(~s): ~s~n",[ClientId, emqx_message:format(Message)]),
  Topic = Message#message.topic,
  Payload = Message#message.payload,
  Qos = Message#message.qos,
  From = Message#message.from,
  Timestamp = Message#message.timestamp,
  Content = [
    {action, <<"message_acked">>},
    {from, From},
    {to, ClientId},
    {topic, Topic},
    {payload, Payload},
    {qos, Qos},
    {cluster_node, node()},
    {ts, Timestamp}
  ],
  produce_kafka_payload(ClientId, Content),
  ok.

%%--------------------------------------------------------------------
%% Session Lifecircle Hooks
%%--------------------------------------------------------------------

on_session_created(#{clientid := ClientId}, SessInfo, _Env) ->
  %% ?LOG_INFO("[KAFKA PLUGIN]Session(~s) created, Session Info:~n~p~n", [ClientId, SessInfo]),
  ok.


on_session_subscribed(#{clientid := ClientId}, Topic, SubOpts, _Env) ->
  %% ?LOG_INFO("[KAFKA PLUGIN]Session(~s) subscribed ~s with subopts: ~p~n", [ClientId, Topic, SubOpts]),
  ok.

on_session_unsubscribed(#{clientid := ClientId}, Topic, Opts, _Env) ->
  %% ?LOG_INFO("[KAFKA PLUGIN]Session(~s) unsubscribed ~s with opts: ~p~n", [ClientId, Topic, Opts]),
  ok.

on_session_resumed(#{clientid := ClientId}, SessInfo, _Env) ->
  %% ?LOG_INFO("[KAFKA PLUGIN]Session(~s) resumed, Session Info:~n~p~n", [ClientId, SessInfo]),
  ok.

on_session_discarded(_ClientInfo = #{clientid := ClientId}, SessInfo, _Env) ->
  %% ?LOG_INFO("[KAFKA PLUGIN]Session(~s) is discarded. Session Info: ~p~n", [ClientId, SessInfo]),
  ok.

on_session_takeovered(_ClientInfo = #{clientid := ClientId}, SessInfo, _Env) ->
  %% ?LOG_INFO("[KAFKA PLUGIN]Session(~s) is takeovered. Session Info: ~p~n", [ClientId, SessInfo]),
  ok.

on_session_terminated(_ClientInfo = #{clientid := ClientId}, Reason, SessInfo, _Env) ->
  %% ?LOG_INFO("[KAFKA PLUGIN]Session(~s) is terminated due to ~p~nSession Info: ~p~n",[ClientId, Reason, SessInfo]),
  ok.

kafka_init(_Env) ->
  %% ?LOG_INFO("Start to init emqx plugin kafka..... ~n"),
  {ok, AddressList} = application:get_env(emqx_plugin_kafka, kafka_address_list),
  %% ?LOG_INFO("[KAFKA PLUGIN]KafkaAddressList = ~p~n", [AddressList]),
  {ok, KafkaConfig} = application:get_env(emqx_plugin_kafka, kafka_config),
  %% ?LOG_INFO("[KAFKA PLUGIN]KafkaConfig = ~p~n", [KafkaConfig]),
  {ok, KafkaTopic} = application:get_env(emqx_plugin_kafka, topic),
  %% ?LOG_INFO("[KAFKA PLUGIN]KafkaTopic = ~s~n", [KafkaTopic]),
  %%{ok, _} = application:ensure_all_started(brod),
  %%ok = brod:start_client(AddressList, emqx_repost_worker, KafkaConfig),
  %%ok = brod:start_producer(emqx_repost_worker, KafkaTopic, []),
  {ok, _} = application:ensure_all_started(brod),
  %%KafkaBootstrapEndpoints = [{"192.168.0.4", 9092}],
  Topic = list_to_binary(KafkaTopic),
  %%KafkaBootstrapEndpoints = [{"192.168.0.4", 9092},{"192.168.0.4", 9093},{"192.168.0.4", 9094}],
  ok = brod:start_client(AddressList, client1),
  ok = brod:start_producer(client1,Topic, _ProducerConfig = []),
  %% ?LOG_INFO("Init emqx plugin kafka successfully.....~n"),
  ok.

get_kafka_topic() ->
  {ok, Topic} = application:get_env(emqx_plugin_kafka, topic),
  Topic.


format_payload(Message) ->
  Username = emqx_message:get_header(username, Message),
  Topic = Message#message.topic,
  Tail = string:right(binary_to_list(Topic), 4),
  RawType = string:equal(Tail, <<"_raw">>),
  % ?LOG_INFO("[KAFKA PLUGIN]Tail= ~s , RawType= ~s~n",[Tail,RawType]),
  ClientId = Message#message.from,
  MsgPayload = Message#message.payload,
  % ?LOG_INFO("[KAFKA PLUGIN]MsgPayload : ~s~n", [MsgPayload]),
  if
    RawType == true ->
      MsgPayload64 = list_to_binary(base64:encode_to_string(MsgPayload));
  % ?LOG_INFO("[KAFKA PLUGIN]MsgPayload64 : ~s~n", [MsgPayload64]);
    RawType == false ->
      MsgPayload64 = MsgPayload
  end,
  Payload = [{action, message_publish},
    {device_id, ClientId},
    {username, Username},
    {topic, Topic},
    {payload, MsgPayload64},
    {ts, Message#message.timestamp}],

  {ok, ClientId, Payload}.


%% Called when the plugin application stop
unload() ->
  emqx:unhook('client.connect', {?MODULE, on_client_connect}),
  emqx:unhook('client.connack', {?MODULE, on_client_connack}),
  emqx:unhook('client.connected', {?MODULE, on_client_connected}),
  emqx:unhook('client.disconnected', {?MODULE, on_client_disconnected}),
  emqx:unhook('client.authenticate', {?MODULE, on_client_authenticate}),
  emqx:unhook('client.check_acl', {?MODULE, on_client_check_acl}),
  emqx:unhook('client.subscribe', {?MODULE, on_client_subscribe}),
  emqx:unhook('client.unsubscribe', {?MODULE, on_client_unsubscribe}),
  emqx:unhook('session.created', {?MODULE, on_session_created}),
  emqx:unhook('session.subscribed', {?MODULE, on_session_subscribed}),
  emqx:unhook('session.unsubscribed', {?MODULE, on_session_unsubscribed}),
  emqx:unhook('session.resumed', {?MODULE, on_session_resumed}),
  emqx:unhook('session.discarded', {?MODULE, on_session_discarded}),
  emqx:unhook('session.takeovered', {?MODULE, on_session_takeovered}),
  emqx:unhook('session.terminated', {?MODULE, on_session_terminated}),
  emqx:unhook('message.publish', {?MODULE, on_message_publish}),
  emqx:unhook('message.delivered', {?MODULE, on_message_delivered}),
  emqx:unhook('message.acked', {?MODULE, on_message_acked}),
  emqx:unhook('message.dropped', {?MODULE, on_message_dropped}).

produce_kafka_payload(Key, Message) ->
  Topic = list_to_binary(get_kafka_topic()),
  {ok, MessageBody} = emqx_json:safe_encode(Message),
  %% ?LOG_INFO("[KAFKA PLUGIN]Message = ~s~n",[MessageBody]),
  %% ?LOG_INFO("[KAFKA PLUGIN]Topic = ~s~n",[Topic]),
  Payload = iolist_to_binary(MessageBody),
  %%brod:produce_cb(client1, Topic, hash, Key, Payload, fun(_,_) -> ok end),
  AckCb = fun(Partition, BaseOffset) -> io:format(user, "\nProduced to partition ~p at base-offset ~p\n", [Partition, BaseOffset]) end,
  {ok ,_}= brod:produce_cb(client1, Topic, hash, Key, Payload, AckCb).

ntoa({0, 0, 0, 0, 0, 16#ffff, AB, CD}) ->
  inet_parse:ntoa({AB bsr 8, AB rem 256, CD bsr 8, CD rem 256});
ntoa(IP) ->
  inet_parse:ntoa(IP).
now_mill_secs({MegaSecs, Secs, _MicroSecs}) ->
  MegaSecs * 1000000000 + Secs * 1000 + _MicroSecs.

hook(HookPoint, MFA) ->
    %% use highest hook priority so this module's callbacks
    %% are evaluated before the default hooks in EMQX
    emqx_hooks:add(HookPoint, MFA, _Property = ?HP_HIGHEST).

unhook(HookPoint, MFA) ->
    emqx_hooks:del(HookPoint, MFA).
