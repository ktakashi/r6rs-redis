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

echo Testing contrib

# Using find command to be portable (for OSX which uses old bash...)
FIND=`which find`
loadpaths=`cat .deps`
for file in `${FIND} './contrib' -name 'test-info.sh'`; do
    dir=$(dirname "${file}")
    source ${file}
    eval declare -a contrib_impls=${IMPLEMENTATIONS}
    for impl in ${contrib_impls[@]}; do
	echo Testing ${dir} on ${impl}
	for file in ${dir}/${TEST_DIR}/*.scm; do
 	    scheme-env run ${impl} ${loadpaths} --loadpath lib \
		       --loadpath ${dir}/${LIB_DIR} \
 		       --standard r6rs --program ${file} | check_output
 	    case ${EXIT_STATUS} in
 		0) EXIT_STATUS=$? ;;
 	    esac
	done	
    done
done


exit ${EXIT_STATUS}
