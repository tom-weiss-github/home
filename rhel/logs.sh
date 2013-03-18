#!/bin/bash
function lssr()
{
    local directory=/var/lib/order-connector
    if [ "$1" == "bb1" ]; then
        directory=~/bb1/var/lib/order-connector
    fi
    /bin/ls -tr $directory/*_send_recv_* | tail -1
}

function tlsr()
{
    lssr "$1"
    local directory=/var/lib/order-connector
    if [ "$1" == "bb1" ]; then
        directory=~/bb1/var/lib/order-connector
    fi
    /bin/ls -tr $directory/*_send_recv_* | tail -1 | xargs tail -f | sed -u "s/\x01/  /g" | grep --line-buffered -v 35=0
}

function tlsrh()
{
    lssr "$1"
    local directory=/var/lib/order-connector
    if [ "$1" == "bb1" ]; then
        directory=~/bb1/var/lib/order-connector
    fi
    /bin/ls -tr $directory/*_send_recv_* | tail -1 | xargs tail -f | sed -u "s/\x01/  /g"
}

function edsr()
{
    local directory=/var/lib/order-connector
    if [ "$1" == "bb1" ]; then
        directory=~/bb1/var/lib/order-connector
    fi
    emacs -nw `/bin/ls -tr $directory/*_send_recv_* | tail -1`
}

function lsoc()
{
    local log_files=/var/log/*cme*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*cme*
    fi
    /bin/ls -tr $log_files | tail -1
}

function tloc()
{
    local log_files=/var/log/*cme*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*cme*
    fi
    /bin/ls -tr $log_files | tail -1 | xargs tail -f
}

function edoc()
{
    local log_files=/var/log/*cme*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*cme*
    fi
    emacs -nw `/bin/ls -tr $log_files | tail -1`
}

function rmoc()
{
    local log_files=/var/log/*cme*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*cme*
    fi
    rm -v `/bin/ls -tr $log_files | tail -1`
}

function lslu()
{
    local log_files=/var/log/*ledgerupdateserver*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*ledgerupdateserver*
    fi
    /bin/ls /bin/ls -tr $log_files | tail -1
}

function tllu()
{
    local log_files=/var/log/*ledgerupdateserver*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*ledgerupdateserver*
    fi
    /bin/ls -tr /bin/ls -tr $log_files | tail -1 | xargs tail -f
}

function edlu()
{
    local log_files=/var/log/*ledgerupdateserver*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*ledgerupdateserver*
    fi
    emacs -nw `/bin/ls -tr $log_files | tail -1`
}

function rmlu()
{
    local log_files=/var/log/*ledgerupdateserver*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*ledgerupdateserver*
    fi
    rm -v `/bin/ls -tr $log_files | tail -1`
}

function lslr()
{
    local log_files=/var/log/*ledgerrequestserver*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*ledgerupdateserver*
    fi
    /bin/ls /bin/ls -tr $log_files | tail -1
}

function tllr()
{
    local log_files=/var/log/*ledgerrequestserver*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*ledgerrequestserver*
    fi
    /bin/ls -tr /bin/ls -tr $log_files | tail -1 | xargs tail -f
}

function edlr()
{
    local log_files=/var/log/*ledgerrequestserver*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*ledgerrequestserver*
    fi
    emacs -nw `/bin/ls -tr $log_files | tail -1`
}

function rmlr()
{
    local log_files=/var/log/*ledgerrequestserver*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*ledgerrequestserver*
    fi
    rm -v `/bin/ls -tr $log_files | tail -1`
}