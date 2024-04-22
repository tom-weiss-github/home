#!/bin/bash

OUTPUT=/var/log/topmem.log
touch $OUTPUT

echo "" >> $OUTPUT
echo "--------------------PROCESS MEMORY SUMMED BY NAME--------------------" >> $OUTPUT
date --utc +%FT%TZ >> $OUTPUT
# This option is great to sum all the instances of the processes.  However, that throws away all the
# parameters.  So it's not great if you want to know which instance is consuming the memory.
ps -e -orss=,args= |awk '{print $1 " " $2 }'| awk '{tot[$2]+=$1;count[$2]++} END {for (i in tot) {print tot[i],i,count[i]}}' | sort -n | tail -n 100 | sort -nr | awk '{ hr=$1/1024; printf("%13.2fM", hr); print "\t" $2 }' >> $OUTPUT

echo "" >> $OUTPUT
echo "--------------------PROCESS MEMORY WITH ARGUMENTS--------------------" >> $OUTPUT
date --utc +%FT%TZ >> $OUTPUT
ps -e -orss=,args= | sort -n | tail -n 13 | awk '{ hr=$1/1024; printf("%13.2fM", hr); print "\t" $2 " " $3 " " $4 " " $5 " " $6 " " $7 " " $8 " " $9 " " $10}' | tac >> $OUTPUT
