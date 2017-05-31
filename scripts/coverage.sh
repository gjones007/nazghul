#!/bin/sh
#
# Code coverage for haxima.
#
# Reconfigure haxima with coverage flags, rebuild, run and then run
# coverage tools. Results will be in ../test_coverage,
# e.g. ../test_coverage/index.html.
#
# Run this from this directory.
#

which lcov 1>/dev/null 2>&1
if [ $? != 0 ]
then
    echo "You need to have lcov installed in order to generate the test coverage report"
    exit 1
fi

cd ..

# Reconfigure with gcov support
CXXFLAGS="-g -O0 --coverage" CFLAGS="-g -O0 --coverage" ./autogen.sh --disable-shared

# Generate gcov output
${MAKE} install

# Reset lcov for test run
lcov --base-directory . --directory . --zerocounters -q

# Run the test
cd worlds/haxima-1.002
nazghul
cd ../../src

# Generate html report
lcov --base-directory . --directory . -c -o nazghul_test.info
lcov --remove nazghul_test.info "/usr*" -o nazghul_test.info # remove output for external libraries
rm -rf ../test_coverage
genhtml -o ../test_coverage -t "libbash test coverage" --num-spaces 4 nazghul_test.info
