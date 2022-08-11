rebar3 etcd v3 proto service generate plugin
=====

A rebar3 plugin for generating a behaviour etcd v3 service, for use with [eetcd](https://github.com/zhongwencool/eetcd).

Build
-----

```
rebar3 compile
```

Use
---

Add the plugin to your rebar config:

``` erlang
{deps, [eetcd]}.

{plugins, [rebar3_gbp_plugin, rebar3_eetcd_plugin]}.
```

and then run generation:

```shell
rebar3 etcd gen
```
