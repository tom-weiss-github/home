#!/bin/bash
logger_prefix="mount_workstation"
output_file=/tmp/sshfs_output
hostname=gld2vm30
ip=10.195.2.30

echo "running mount_workstation.sh"
mkdir -p ~/mnt/${hostname}

while true; do
    ssh ${ip} true > /dev/null 2>&1
    exit_code=$?
    if [[ $exit_code -eq 0 ]]; then
        logger -t $logger_prefix "Connected to ${hostname}."
        rm $output_file > /dev/null 2>&1
        mountpoint /home/tweiss/mnt/${hostname} > /dev/null 2>&1
        exit_code=$?
        if [[ $exit_code -ne 0 ]]; then
            logger -t $logger_prefix "Mounting home on ${hostname}, not currently mounted."
            umount /home/tweiss/mnt/gld1vm89 > /dev/null 2>&1
            sshfs -o allow_other tweiss@${ip}:/home/tweiss /home/tweiss/mnt/${hostname} -o IdentityFile=/home/tweiss/.ssh/id_rsa > $output_file 2>&1
            exit_code=$?
            if [[ $exit_code -eq 0 ]]; then
                logger -t $logger_prefix "Successfully mounted ${hostname}."
            else
                logger -t $logger_prefix "Failed to mount ${hostname}."
                logger -t $logger_prefix `cat $output_file`
            fi
        fi
    else
        logger -t $logger_prefix "Not able to connect to ${hostname}."
    fi
    sleep 5
done
