#!/bin/bash

function lssr_()
{
    local directory=/var/lib/order-connector
    if [ "$1" == "bb1" ]; then
        directory=~/bb1/var/lib/order-connector
    fi
    /bin/ls -tr $directory/*_send_recv_* | tail -1
}

function tlsr_()
{
    lssr_ "$1"
    local directory=/var/lib/order-connector
    if [ "$1" == "bb1" ]; then
        directory=~/bb1/var/lib/order-connector
    fi
    /bin/ls -tr $directory/*_send_recv_* | tail -1 | xargs tail -f | sed -u "s/\x01/  /g" | grep --line-buffered -v 35=0
}

function tlsrh_()
{
    lssr_ "$1"
    local directory=/var/lib/order-connector
    if [ "$1" == "bb1" ]; then
        directory=~/bb1/var/lib/order-connector
    fi
    /bin/ls -tr $directory/*_send_recv_* | tail -1 | xargs tail -f | sed -u "s/\x01/  /g"
}

function edsr_()
{
    local directory=/var/lib/order-connector
    if [ "$1" == "bb1" ]; then
        directory=~/bb1/var/lib/order-connector
    fi
    emacs -nw `/bin/ls -tr $directory/*_send_recv_* | tail -1`
}

function lsoc_()
{
    local log_files=/var/log/*cme*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*cme*
    fi
    /bin/ls -tr $log_files | tail -1
}

function tloc_()
{
    local log_files=/var/log/*cme*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*cme*
    fi
    /bin/ls -tr $log_files | tail -1 | xargs tail -f
}

function edoc_()
{
    local log_files=/var/log/*cme*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*cme*
    fi
    emacs -nw `/bin/ls -tr $log_files | tail -1`
}

function rmoc_()
{
    local log_files=/var/log/*cme*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*cme*
    fi
    rm -v `/bin/ls -tr $log_files | tail -1`
}


