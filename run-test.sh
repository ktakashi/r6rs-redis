#!/bin/bash

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

declare -a implementations=(sagittarius@0.9.4 chez@v9.5)

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
		   --standard r6rs --program ${file} 2>&1 | check_output
	case ${EXIT_STATUS} in
	    0) EXIT_STATUS=$? ;;
	esac
    done
    echo Done!
    echo
done
cd ..

echo Library test status ${EXIT_STATUS}

if [ ${EXIT_STATUS} != 0 ]; then
    exit ${EXIT_STATUS}
fi

/bin/bash ./run-contrib-test.sh
exit $?
