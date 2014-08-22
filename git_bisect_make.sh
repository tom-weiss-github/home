#!/bin/bash

make --silent -Rr -j 7 clean
make --silent -Rr -j 7
exit $?