#!/bin/sh

/bin/sh ./tools/docker.sh start

/bin/bash ./run-test.sh
status=$?

/bin/sh ./tools/docker.sh stop

exit $status
