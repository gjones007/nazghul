#!/bin/sh
#
# Launch the smoke test.
#
# Run from topdir.
#
set -e
cd worlds/haxima-1.003
rm -rf saves
nazghul -P smoke.rec haxima.scm
if [ $? = 0 ]; then
    echo "PASS"
else
    echo "FAIL"
fi
cd ../..
