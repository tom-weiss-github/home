#!/bin/bash
logger_prefix="mount_workstation"
output_file=/tmp/sshfs_output

while true; do
    ssh 10.195.1.89 true > /dev/null 2>&1
    exit_code=$?
    if [[ $exit_code -eq 0 ]]; then
        logger -t $logger_prefix "Connected to gld."
        rm $output_file > /dev/null 2>&1
        mountpoint /home/tweiss/mnt/gld1vm89 > /dev/null 2>&1
        exit_code=$?
        if [[ $exit_code -ne 0 ]]; then
            logger -t $logger_prefix "Mounting home on gld1vm89, not currently mounted."
            umount /home/tweiss/mnt/gld1vm89 > /dev/null 2>&1
            sshfs -o allow_other tweiss@10.195.1.89:/home/tweiss /home/tweiss/mnt/gld1vm89 -o IdentityFile=/home/tweiss/.ssh/id_rsa > $output_file 2>&1
            exit_code=$?
            if [[ $exit_code -eq 0 ]]; then
                logger -t $logger_prefix "Successfully mounted gld1vm89."
            else
                logger -t $logger_prefix "Failed to mount gld1vm89."
                logger -t $logger_prefix `cat $output_file`
            fi
        fi
    else
        logger -t $logger_prefix "Not able to connect to gld."
    fi
    sleep 5
done
