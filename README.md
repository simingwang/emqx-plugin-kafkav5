# emqx-plugin-template

This is a template plugin for EMQX >= 5.0.

For EMQX >= 4.3, please see branch emqx-v4

For older EMQX versions, plugin development is no longer maintained.

## Release

A EMQX plugin release is a zip package including

1. A JSON format metadata file
2. A tar file with plugin's apps packed

Execute `make rel` to have the package created like:

```
_build/default/emqx_plugrel/emqx_plugin_template-<vsn>.tar.gz
```
If deploying emqx with build from source.
See EMQX documents for details on how to deploy the plugin.

1. need to set NIF_BIN_DIR to /usr/lib64  
   text "export NIF_BIN_DIR = /ust/lib64" to  /etc/profile 
2. put all plugin's  *.so file to  /usr/lib64

######kafka configure in /etc/profile
set up kafka configuration in env file
```
export KAFKA_ADDRESS_LIST=192.168.1.1:9092,192.168.1.2:9092,192.168.1.3:9092
export KAFKA_TOPIC=mqtt-topic
export KAFKA_QUERY_API_VERSIONS=true
export KAFKA_RECONNECT_COOL_DOWN_SECONDS=10
```




