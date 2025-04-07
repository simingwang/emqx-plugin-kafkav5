# emqx-plugin-template

This is a template plugin for EMQX >= 5.0.

For EMQX >= 4.3, please see branch emqx-v4

For older EMQX versions, plugin development is no longer maintained.

## Release

A EMQX plugin release is a zip package including

1. A JSON format metadata file
2. A tar file with plugin's apps packed

Ensure you have installed OTP 24+ ,git 2.x ,cmake 3.4+ in your environment.

```bash
make rel
```

Execute `make rel` in the project folder , if succeeded ,you will have the package created like:

```
_build/default/emqx_plugrel/emqx_plugin_kafka-5.0.0.tar.gz
```

You can directly install above package to emqx5+ plugins dashboard, and following below guides to setup the env variables.

Set up the the extenal config file , it is from a fixed path: /etc/emqx_kafka.conf. The following is an example, mqtt_topics defines the mqtt topics to publish to kafka.

```
kafka {
  address_list = "127.0.0.1:9092" # kafka address list, multiple addresses are separated by commas
  reconnect_cool_down_seconds = 15
  query_api_versions = true
  topic = "mqtt-events" # kafka topic to publish
  mqtt_topics  = [ "/data/#" ]
}
```

Alternatively, you can set up kafka configuration in /etc/profile, if there is not extenal config, the plugin will read the configuration from /etc/profile.

set up kafka configuration in env file, 
```
export KAFKA_ADDRESS_LIST=192.168.1.1:9092 # kafka address list, multiple addresses are separated by commas
export KAFKA_TOPIC=mqtt-topic # kafka topic to publish
export KAFKA_QUERY_API_VERSIONS=true
export KAFKA_RECONNECT_COOL_DOWN_SECONDS=10
export KAFKA_MQTT_TOPICS=/data/#,/data2/#
```
Note: If you are using docker, you need to use -e to specify the env variables or  mount the extenal config file to the container.



