#!/bin/sh

DDIR="./deps"
ROOTDIR=`pwd`
DEPSDIR="$ROOTDIR/$DDIR"

RABBIT_VERSION="1.8.0"

mkdir -p $DEPSDIR
cd $DEPSDIR

# Make sure rabbitmq-server is available
(  
  RABBIT_TAR="rabbitmq-server-$RABBIT_VERSION.tar.gz"
  if [ ! -d "$DEPSDIR/rabbitmq-server" ]; then
    curl -O "http://www.rabbitmq.com/releases/rabbitmq-server/v$RABBIT_VERSION/$RABBIT_TAR"
    tar -zxf $RABBIT_TAR
    mv "rabbitmq-server-$RABBIT_VERSION" "rabbitmq-server"
    rm $RABBIT_TAR
  fi
  cd "rabbitmq-server"
  make
)

# Make sure rabbitmq-erlang-client is available
(
  if [ ! -d "$DEPSDIR/rabbitmq-erlang-client" ]; then
    hg clone http://hg.rabbitmq.com/rabbitmq-erlang-client/
  fi
  cd rabbitmq-erlang-client
  make 
)

# Download rebar deps
(
  make get-deps
)