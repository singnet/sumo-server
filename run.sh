#!/bin/bash

tag="${2:+:$2}"
label="${2:+-$2}"
image="sumo-server$tag"
container="sumo-server$label"
docker-run () {
  docker run --name $container --stop-timeout 30 $@
}

help () {
  echo "Usage: bash run.sh command [build-label] [port-number]"
  echo ""
  echo "Commands:"
  echo "  build  Build the SUMO server."
  echo "  shell  Start a container and drop into the shell without starting"
  echo "         the sumo server."
  echo "  start  Start the container as a background process with the"
  echo "         sumo server running and port 7083 exposed locally."
}

case $1 in
  build) docker build -t $image . ;;
  start) docker-run -w /opt/sumo-server -p 7083:9999 \
         --rm -d $image \
         guile --no-auto-compile sumo-server.scm ;;
  shell) docker-run --rm -it -p 7083:9999 $image bash ;;
  stop) docker stop $container ;;
  *) help ;;
esac
