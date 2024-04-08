#!/bin/bash

OUTPUT=/var/log/topmem.log
touch $OUTPUT

date --utc +%FT%TZ >> $OUTPUT
ps -e -orss=,args= |awk '{print $1 " " $2 }'| awk '{tot[$2]+=$1;count[$2]++} END {for (i in tot) {print tot[i],i,count[i]}}' | sort -n | tail -n 100 | sort -nr | awk '{ hr=$1/1024; printf("%13.2fM", hr); print "\t" $2 }' >> $OUTPUT
