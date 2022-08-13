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
  kafka_init(Env),
  hook('client.connect',      {?MODULE, on_client_connect, [Env]}),
  hook('client.connack',      {?MODULE, on_client_connack, [Env]}),
  hook('client.connected',    {?MODULE, on_client_connected, [Env]}),
  hook('client.disconnected', {?MODULE, on_client_disconnected, [Env]}),
  hook('client.authenticate', {?MODULE, on_client_authenticate, [Env]}),
  hook('client.authorize',    {?MODULE, on_client_authorize, [Env]}),
  hook('client.check_acl',    {?MODULE, on_client_check_acl, [Env]}),
  hook('client.subscribe',    {?MODULE, on_client_subscribe, [Env]}),
  hook('client.unsubscribe',  {?MODULE, on_client_unsubscribe, [Env]}),
  hook('session.created',     {?MODULE, on_session_created, [Env]}),
  hook('session.subscribed',  {?MODULE, on_session_subscribed, [Env]}),
  hook('session.unsubscribed',{?MODULE, on_session_unsubscribed, [Env]}),
  hook('session.resumed',     {?MODULE, on_session_resumed, [Env]}),
  hook('session.discarded',   {?MODULE, on_session_discarded, [Env]}),
  hook('session.takeovered',  {?MODULE, on_session_takeovered, [Env]}),
  hook('session.terminated',  {?MODULE, on_session_terminated, [Env]}),
  hook('message.publish',     {?MODULE, on_message_publish, [Env]}),
  hook('message.delivered',   {?MODULE, on_message_delivered, [Env]}),
  hook('message.acked',       {?MODULE, on_message_acked, [Env]}),
  hook('message.dropped',     {?MODULE, on_message_dropped, [Env]}).

on_client_connect(ConnInfo = #{clientid := ClientId}, Props, _Env) ->
  ?SLOG(debug, #{msg => "demo_log_msg_on_client_connect",
                   conninfo => ConnInfo,
                   props => Props}),
  {ok, Props}. 

on_client_connack(ConnInfo = #{clientid := ClientId}, Rc, Props, _Env) ->
  io:format("Client(~s) connack, ConnInfo: ~p, Rc: ~p, Props: ~p~n",
              [ClientId, ConnInfo, Rc, Props]),
  {ok, Props}.

on_client_connected(ClientInfo = #{clientid := ClientId}, ConnInfo, _Env) ->
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
  io:format("Client(~s) connected, ClientInfo:~n~p~n, ConnInfo:~n~p~n", [ClientId, ClientInfo, ConnInfo]).

on_client_disconnected(ClientInfo = #{clientid := ClientId}, ReasonCode, ConnInfo, _Env) ->
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
  io:format("Client(~s) disconnected due to ~p, ClientInfo:~n~p~n, ConnInfo:~n~p~n", [ClientId, ReasonCode, ClientInfo, ConnInfo]).

on_client_authenticate(ClientInfo = #{clientid := ClientId}, Result, Env) ->
io:format("Client(~s) authenticate, ClientInfo:~n~p~n, Result:~p,~nEnv:~p~n", [ClientId, ClientInfo, Result, Env]),
  {ok, Result}.

on_client_authorize(ClientInfo = #{clientid := ClientId}, PubSub, Topic, Result, Env) ->
  io:format("Client(~s) authorize, ClientInfo:~n~p~n, ~p to topic(~s) Result:~p,~nEnv:~p~n", [ClientId, ClientInfo, PubSub, Topic, Result, Env]),
  {ok, Result}.

%%---------------------------client subscribe start--------------------------%%
on_client_subscribe(#{clientid := ClientId}, _Properties, TopicFilters, _Env) ->
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
  io:format("Client(~s) will subscribe: ~p~n", [ClientId, TopicFilters]),
  {ok, TopicFilters}.

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
  io:format("Client(~s) will unsubscribe ~p~n", [ClientId, TopicFilters]),
  {ok, TopicFilters}.

%%--------------------------------------------------------------------
%% Message PubSub Hooks
%%--------------------------------------------------------------------

%% Transform message and return
on_message_dropped(#message{topic = <<"$SYS/", _/binary>>}, _By, _Reason, _Env) ->
ok;
  
on_message_dropped(Message, _By = #{node := Node}, Reason, _Env) ->
io:format("Message dropped by node ~p due to ~p:~n~p~n",[Node, Reason, emqx_message:to_map(Message)]).


%%---------------------------message publish start--------------------------%%
on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}, _Env) ->
  {ok, Message};
on_message_publish(Message, _Env) ->
  {ok, ClientId, Payload} = format_payload(Message),
  produce_kafka_payload(ClientId, Payload),
  io:format("Publish ~p~n", [emqx_message:to_map(Message)]),
  {ok, Message}.
%%---------------------message publish stop----------------------%%

on_message_delivered(_ClientInfo = #{clientid := ClientId}, Message, _Env) ->
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
  io:format("Message delivered to client(~s):~n~p~n", [ClientId, emqx_message:to_map(Message)]),
  {ok, Message}.

on_message_acked(_ClientInfo = #{clientid := ClientId}, Message, _Env) ->
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
  io:format("Message acked by client(~s):~n~p~n", [ClientId, emqx_message:to_map(Message)]).

%%--------------------------------------------------------------------
%% Session LifeCircle Hooks
%%--------------------------------------------------------------------

on_session_created(#{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) created, Session Info:~n~p~n", [ClientId, SessInfo]).

on_session_subscribed(#{clientid := ClientId}, Topic, SubOpts, _Env) ->
    io:format("Session(~s) subscribed ~s with subopts: ~p~n", [ClientId, Topic, SubOpts]).

on_session_unsubscribed(#{clientid := ClientId}, Topic, Opts, _Env) ->
    io:format("Session(~s) unsubscribed ~s with opts: ~p~n", [ClientId, Topic, Opts]).

on_session_resumed(#{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) resumed, Session Info:~n~p~n", [ClientId, SessInfo]).

on_session_discarded(_ClientInfo = #{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) is discarded. Session Info: ~p~n", [ClientId, SessInfo]).

on_session_takeovered(_ClientInfo = #{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) is takeovered. Session Info: ~p~n", [ClientId, SessInfo]).

on_session_terminated(_ClientInfo = #{clientid := ClientId}, Reason, SessInfo, _Env) ->
    io:format("Session(~s) is terminated due to ~p~nSession Info: ~p~n", [ClientId, Reason, SessInfo]).

kafka_init(_Env) ->
  io:format("Start to init emqx plugin kafka..... ~n"),
  AddressList = translate(maps:get(addresslist, _Env)),
  io:format("[KAFKA PLUGIN]KafkaAddressList = ~p~n", [AddressList]),
  %ReconnectCoolDownSeconds = maps:get(reconnectcooldownseconds,_Env),
  %QueryApiVersions = maps:get(queryapiversions,_Env),
  % io:format("[KAFKA PLUGIN]KafkaConfig = ~p~n", [KafkaConfig]),
  KafkaTopic = list_to_binary(maps:get(topic, _Env)),
  io:format("[KAFKA PLUGIN]KafkaTopic = ~s~n", [KafkaTopic]),
  %%{ok, _} = application:ensure_all_started(brod),
  %%ok = brod:start_client(AddressList, emqx_repost_worker, KafkaConfig),
  %%ok = brod:start_producer(emqx_repost_worker, KafkaTopic, []),
  {ok, _} = application:ensure_all_started(brod),
  %%KafkaBootstrapEndpoints = [{"192.168.0.4", 9092}],
  %Topic = list_to_binary(KafkaTopic),
  %Topic = <<"test-topic">>,
  %KafkaBootstrapEndpoints = [{"192.168.0.4", 9092},{"192.168.0.4", 9093},{"192.168.0.4", 9094}],
  ok = brod:start_client(AddressList, client1),
  ok = brod:start_producer(client1, , _ProducerConfig = []),
  io:format("Init emqx plugin kafka successfully.....~n").

get_kafka_topic() ->
  %{ok, Topic} = application:get_env(emqx_plugin_kafka, topic),
  list_to_binary(os:getenv("KAFKA_TOPIC")).


format_payload(Message) ->
  Username = emqx_message:get_header(username, Message),
  Topic = Message#message.topic,
  Tail = string:right(binary_to_list(Topic), 4),
  RawType = string:equal(Tail, <<"_raw">>),
  io:format("[KAFKA PLUGIN]Tail= ~s , RawType= ~s~n",[Tail,RawType]),
  ClientId = Message#message.from,
  MsgPayload = Message#message.payload,
  io:format("[KAFKA PLUGIN]MsgPayload : ~s~n", [MsgPayload]),
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
  Topic = get_kafka_topic(),
  {ok, MessageBody} = emqx_json:safe_encode(Message),
  io:format("[KAFKA PLUGIN]Message = ~s~n",[MessageBody]),
  io:format("[KAFKA PLUGIN]Topic = ~s~n",[Topic]),
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

translate(AddressList) ->
  Fun = fun(S) ->
    case string:split(S, ":", trailing) of
      [Domain]       -> {Domain, 9092};
      [Domain, Port] -> {Domain, list_to_integer(Port)}
    end
  end,
  S = string:tokens(AddressList, ","),
  [Fun(S1) || S1 <- S].

hook(HookPoint, MFA) ->
    %% use highest hook priority so this module's callbacks
    %% are evaluated before the default hooks in EMQX
    emqx_hooks:add(HookPoint, MFA, _Property = ?HP_HIGHEST).

unhook(HookPoint, MFA) ->
    emqx_hooks:del(HookPoint, MFA).


