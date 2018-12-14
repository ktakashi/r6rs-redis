#!/bin/sh

redis_name=test_redis_server

echo "Starting Redis server: $redis_name"
docker run --name $redis_name -d -p 6379:6379 redis

/bin/sh ./run-test.sh
status=$?

echo "Stopping Redis server: $redis_name"
docker stop $redis_name
docker rm $redis_name

exit $status
