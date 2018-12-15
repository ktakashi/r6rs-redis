#!/bin/sh

redis_name=test_redis_server

case $1 in
    start) 
	echo "Starting Redis server: $redis_name"
	docker run --name $redis_name -d -p 6379:6379 redis
	;;
    stop)
	echo "Stopping Redis server: $redis_name"
	docker stop $redis_name
	docker rm $redis_name
	;;
    *)
	echo "$0 start|stop"
	exit -1
	;;
esac
