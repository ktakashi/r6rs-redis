#!/bin/sh

/bin/sh ./docker.sh start

/bin/sh ./run-test.sh
status=$?

/bin/sh ./docker.sh stop

exit $status
