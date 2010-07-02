#!/bin/sh

# Starting rabbitmq
echo "Starting rabbithole"
exec erl -pa ./ebin \
    -pa ./deps/rabbitmq-server/ebin \
    -pa ./deps/rabbitmq-erlang-client/ebin \
    -boot start_sasl \
    -eval "application:start(rabbithole)"