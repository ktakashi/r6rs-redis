#!/bin/sh

echo 'Updating dependencies'
git submodule update --init

echo 'Updating usocket dependencies'
cd deps/usocket
git submodule update --init

echo 'Creating usocket constant'
make

cd ../../

echo 'Creating .deps file'
echo ' --loadpath deps/usocket/lib' > .deps
echo ' --loadpath deps/usocket/deps/pffi/src' >> .deps
echo ' --loadpath deps/usocket/deps/psystem/lib' >> .deps
