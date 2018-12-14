#!/bin/sh

if [ ! -f '.deps' ]; then
    ./setup-deps.sh
fi

echo "Preparing dependencies"
cd test-deps/testing
./setup.sh
cd ../../

if ! grep -q 'test-deps/testing/lib' '.deps'; then
    echo " --loadpath test-deps/testing/lib" >> .deps
fi

redis_name=test_redis_server

echo "Starting Redis server: $redis_name"
docker run --name $redis_name -d -p 6379:6379 redis

# actual test

declare -a implementations=(sagittarius@0.9.2 chez@v9.5)

check_output() {
    local status=0
    while IFS= read -r LINE; do
	echo $LINE
	case $LINE in
	    *FAIL*) status=255 ;;
	    *Exception*) status=255 ;;
	esac
    done
    return ${status}
}

EXIT_STATUS=0

loadpaths=`cat .deps`
for impl in ${implementations[@]}; do
    echo Testing with ${impl}
    for file in test/*; do
	scheme-env run ${impl} --loadpath lib ${loadpaths} \
		   --standard r6rs --program ${file} | check_output
	case ${EXIT_STATUS} in
	    0) EXIT_STATUS=$? ;;
	esac
    done
    echo Done!
    echo
done
cd ..

echo Library test status ${EXIT_STATUS}

echo "Stopping Redis server: $redis_name"
docker stop $redis_name
docker rm $redis_name

exit ${EXIT_STATUS}
