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
    local log_files=/var/log/debesys/*cme*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*cme*
    fi
    /bin/ls -tr $log_files | tail -1
}

function tloc()
{
    local log_files=/var/log/debesys/*cme*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*cme*
    fi
    /bin/ls -tr $log_files | tail -1 | xargs tail -f | sed -u "s/\x01/ /g"
}

function edoc()
{
    local log_files=/var/log/debesys/*cme*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*cme*
    fi
    emacs -nw `/bin/ls -tr $log_files | tail -1`
}

function rmoc()
{
    local log_files=/var/log/debesys/*cme*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*cme*
    fi
    rm -v `/bin/ls -tr $log_files | tail -1`
}

function lslu()
{
    local log_files=/var/log/debesys/*ledger_up-*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*ledger_up-*
    fi
    /bin/ls /bin/ls -tr $log_files | tail -1
}

function tllu()
{
    local log_files=/var/log/debesys/*ledger_up-*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*ledger_up-*
    fi
    /bin/ls -tr /bin/ls -tr $log_files | tail -1 | xargs tail -f
}

function edlu()
{
    local log_files=/var/log/debesys/*ledger_up-*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*ledger_up-*
    fi
    emacs -nw `/bin/ls -tr $log_files | tail -1`
}

function rmlu()
{
    local log_files=/var/log/debesys/*ledger_up-*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*ledger_up-*
    fi
    rm -v `/bin/ls -tr $log_files | tail -1`
}

function lslr()
{
    local log_files=/var/log/debesys/*ledger_req-*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*ledger_req-*
    fi
    /bin/ls /bin/ls -tr $log_files | tail -1
}

function tllr()
{
    local log_files=/var/log/debesys/*ledger_req-*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*ledger_req-*
    fi
    /bin/ls -tr /bin/ls -tr $log_files | tail -1 | xargs tail -f
}

function edlr()
{
    local log_files=/var/log/debesys/*ledger_req-*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*ledger_req-*
    fi
    emacs -nw `/bin/ls -tr $log_files | tail -1`
}

function rmlr()
{
    local log_files=/var/log/debesys/*ledger_req-*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*ledger_req-*
    fi
    rm -v `/bin/ls -tr $log_files | tail -1`
}

function lsbr()
{
    local log_files=/var/log/debesys/*bouncerd*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*bouncerd*
    fi
    /bin/ls /bin/ls -tr $log_files | tail -1
}

function tlbr()
{
    local log_files=/var/log/debesys/*bouncerd*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*bouncerd*
    fi
    /bin/ls -tr /bin/ls -tr $log_files | tail -1 | xargs tail -f
}

function edbr()
{
    local log_files=/var/log/debesys/*bouncerd*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*bouncerd*
    fi
    emacs -nw `/bin/ls -tr $log_files | tail -1`
}

function rmbr()
{
    local log_files=/var/log/debesys/*bouncerd*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*bouncerd*
    fi
    rm -v `/bin/ls -tr $log_files | tail -1`
}

function lses()
{
    local log_files=/var/log/debesys/*edgeserver*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*edgeserver*
    fi
    /bin/ls /bin/ls -tr $log_files | tail -1
}

function tles()
{
    local log_files=/var/log/debesys/*edgeserver*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*edgeserver*
    fi
    /bin/ls -tr /bin/ls -tr $log_files | tail -1 | xargs tail -f
}

function edes()
{
    local log_files=/var/log/debesys/*edgeserver*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*edgeserver*
    fi
    emacs -nw `/bin/ls -tr $log_files | tail -1`
}

function rmes()
{
    local log_files=/var/log/debesys/*edgeserver*
    if [ "$1" == "bb1" ]; then
        log_files=~/bb1/var/log/debesys/*edgeserver*
    fi
    rm -v `/bin/ls -tr $log_files | tail -1`
}