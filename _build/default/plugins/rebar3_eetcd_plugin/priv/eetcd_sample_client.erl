%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd {{unmodified_service_name}}
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on {{datetime}} and should not be modified manually

-module(eetcd_{{module_name}}_gen).

{{#methods}}
-export([{{method}}/1]).
{{/methods}}

{{#methods}}
%% @doc {{^output_stream}}{{^input_stream}}Unary RPC for service at path "{{full_service_path}}" {{/input_stream}}{{#input_stream}}Stream RPC {{/input_stream}}{{/output_stream}}
-spec {{method}}(router_pb:'{{input}}'()) ->
    {{^output_stream}}{{^input_stream}}{ok, router_pb:'{{output}}'()}{{/input_stream}}{{#input_stream}}reference(){{/input_stream}}{{/output_stream}}{{#output_stream}}{{^input_stream}}reference(){{/input_stream}}{{#input_stream}}reference(){{/input_stream}}{{/output_stream}}|{error,eetcd:eetcd_error()}.
{{method}}(Request) ->
    {{^output_stream}}{{^input_stream}}eetcd_stream:unary(Request, '{{input}}', <<"{{full_service_path}}">>, '{{output}}'){{/input_stream}}{{#input_stream}}eetcd_stream:new(Request, <<"{{full_service_path}}">>){{/input_stream}}{{/output_stream}}{{#output_stream}}{{^input_stream}}eetcd_stream:new(Request, <<"{{full_service_path}}">>){{/input_stream}}{{/output_stream}}.

{{/methods}}
