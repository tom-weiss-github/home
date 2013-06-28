#!/bin/bash

if [ $# -ne 2 ]
then
    echo "Usage: `basename $0` IP LOG"
    exit
fi

log=$2
touch $log

while[ 1 ]
do
    date >> $log
    ping -c 5 $1 >> $log
done