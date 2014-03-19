#!/bin/bash

function lghelp()
{
    echo "lslg log host"
    echo "tllg log host"
    echo "edlg log host"
    echo "alias2logname help    see log aliases"
    echo "lshosts               see host aliases"
    echo "mlghosts              mount all hosts"
    echo "umlghosts             unmount all hosts"
    echo "visit                 ssh to a host"
}

declare host_alias_ip
declare host_alias_mount
declare log_name

host_aliases=(
    d30
    d31
    d32
    d33
    d34
    d35
    d36
    d54
    d81
    ma31
    ma32
    ma33
    ma35
    ma36
    ma37
)

function host2ip()
{
    if [ "$1" == "d30" ]; then
        host_alias_ip=10.202.0.30
    elif [ "$1" == "d31" ]; then
        host_alias_ip=10.202.0.31
    elif [ "$1" == "d32" ]; then
        host_alias_ip=10.202.0.32
    elif [ "$1" == "d33" ]; then
        host_alias_ip=10.202.0.33
    elif [ "$1" == "d34" ]; then
        host_alias_ip=10.202.0.34
    elif [ "$1" == "d35" ]; then
        host_alias_ip=10.202.0.35
    elif [ "$1" == "d36" ]; then
        host_alias_ip=10.202.0.36
    elif [ "$1" == "d54" ]; then
        host_alias_ip=10.202.0.54
    elif [ "$1" == "ma31" ]; then
        host_alias_ip=10.204.0.31
    elif [ "$1" == "ma32" ]; then
        host_alias_ip=10.204.0.32
    elif [ "$1" == "ma33" ]; then
        host_alias_ip=10.204.0.33
    elif [ "$1" == "ma35" ]; then
        host_alias_ip=10.204.0.35
    elif [ "$1" == "ma36" ]; then
        host_alias_ip=10.204.0.36
    elif [ "$1" == "ma37" ]; then
        host_alias_ip=10.204.0.37
    elif [ "$1" == "d81" ]; then
        host_alias_ip=10.202.0.81
    else
        host_alias_ip=INVALID
    fi
}

function alias2logname()
{
    if [ "$1" == "br" ]; then
        log_name="bouncerd"
    elif [ "$1" == "es" ]; then
        log_name="edgeserver"
    elif [ "$1" == "help" ]; then
        echo "br -> bouncerd"
        echo "es -> edgeserver"
    else
        log_name=$1
    fi
}

function visit()
{
    if [ -z "$1" ]; then
        echo usage: you must pass the host alias
        echo visit d61
        return
    fi

    host2ip $1
    echo ssh root@$host_alias_ip -i ~/.ssh/id_rsa
    ssh root@$host_alias_ip -i ~/.ssh/id_rsa
}

function lslg()
{
    if [ -z "$1" ]; then
        echo usage: you must pass the log name or alias
        echo lslg cme d61
        return
    fi

    # If the second argument is empty, then assume localhost
    if [ -z "$2" ]; then
        ha="local"
    else
        ha=$2
    fi

    alias2logname $1
    ls -lh ~/mnt/$ha/var/log/debesys/$1.log
}

function tllg()
{
    if [ -z "$1" ]; then
        echo usage: you must pass the log name or alias
        echo tllg cme d61
        return
    fi

    # If the second argument is empty, then assume localhost
    if [ -z "$2" ]; then
        ha="local"
    else
        ha=$2
    fi

    alias2logname $1
    tail -f ~/mnt/$ha/var/log/debesys/$1.log
}

function edlg()
{
    if [ -z "$1" ]; then
        echo usage: you must pass the log name or alias
        echo edlg cme d61
        return
    fi

    # If the second argument is empty, then assume localhost
    if [ -z "$2" ]; then
        ha="local"
    else
        ha=$2
    fi

    alias2logname $1
    emacs -nw ~/mnt/$ha/var/log/debesys/$1.log
}

function lshosts()
{
    for ha in ${host_aliases[*]}
    do
        host2ip $ha
        printf "%s %s\n" $ha $host_alias_ip
    done
}

function mlghosts()
{
    for ha in ${host_aliases[*]}
    do
        if [ -d ~/mnt/$ha ] && [ "$(/bin/ls -A ~/mnt/$ha)" ]; then
            printf "%s exists and is not empty, skipping\n" ~/mnt/$ha
            continue
        fi
        host2ip $ha
        mkdir -pv ~/mnt/$ha
        echo sshfs root@$host_alias_ip:/ ~/mnt/$ha
        sshfs root@$host_alias_ip:/ ~/mnt/$ha
    done

    # A special alias that points to the local host.
    if [ ! -d ~/mnt/local ]; then
        ln -vs / ~/mnt/local
    fi
}

function umlghosts()
{
    for ha in ${host_aliases[*]}
    do
        host2ip $ha
        if [ ! -d ~/mnt/$ha ]; then
            printf "%s does not exist, skipping\n" ~/mnt/$ha
            continue
        fi
        fusermount -u ~/mnt/$ha
        rmdir -v ~/mnt/$ha
    done

    if [ -d ~/mnt/local ]; then
        rm -v ~/mnt/local
    fi
}






# function lssr()
# {
#     local directory=/var/lib/order-connector
#     if [ "$1" == "dev61" ]; then
#         directory=~/dev61/var/lib/order-connector
#     elif [ "$1" == "sim73" ]; then
#         directory=~/sim73/var/lib/order-connector
#     fi
#     /bin/ls -tr $directory/*_send_recv_* | tail -1
# }

# function tlsr()
# {
#     lssr "$1"
#     local directory=/var/lib/order-connector
#     if [ "$1" == "dev61" ]; then
#         directory=~/dev61/var/lib/order-connector
#     elif [ "$1" == "sim73" ]; then
#         directory=~/sim73/var/lib/order-connector
#     fi
#     /bin/ls -tr $directory/*_send_recv_* | tail -1 | xargs tail -f | sed -u "s/\x01/  /g" | grep --line-buffered -v 35=0
# }

# function tlsrh()
# {
#     lssr "$1"
#     local directory=/var/lib/order-connector
#     if [ "$1" == "dev61" ]; then
#         directory=~/dev61/var/lib/order-connector
#     elif [ "$1" == "sim73" ]; then
#         directory=~/sim73/var/lib/order-connector
#     fi
#     /bin/ls -tr $directory/*_send_recv_* | tail -1 | xargs tail -f | sed -u "s/\x01/  /g"
# }

# function edsr()
# {
#     local directory=/var/lib/order-connector
#     if [ "$1" == "dev61" ]; then
#         directory=~/dev61/var/lib/order-connector
#     elif [ "$1" == "sim73" ]; then
#         directory=~/sim73/var/lib/order-connector
#     fi
#     emacs -nw `/bin/ls -tr $directory/*_send_recv_* | tail -1`
# }

# function lsoc()
# {
#     local log_files=/var/log/debesys/*cme*
#     if [ "$1" == "dev61" ]; then
#         log_files=~/dev61/var/log/debesys/*cme*
#     elif [ "$1" == "sim73" ]; then
#         log_files=~/sim73/var/log/debesys/*cme*
#     fi
#     /bin/ls -tr $log_files | tail -1
# }

# function tloc()
# {
#     local log_files=/var/log/debesys/*cme*
#     if [ "$1" == "dev61" ]; then
#         log_files=~/dev61/var/log/debesys/*cme*
#     elif [ "$1" == "sim73" ]; then
#         log_files=~/sim73/var/log/debesys/*cme*
#     fi
#     echo ""
#     lsoc $1
#     echo ""
#     /bin/ls -tr $log_files | tail -1 | xargs tail -f | sed -u "s/\x01/ /g"
# }
# alias tloc='tail -f /var/log/debesys/cme.log | sed -u "s/\x01/ /g"'
# alias tlocdev61='tail -f ~/dev61/var/log/debesys/cme.log | sed -u "s/\x01/ /g"'

# function edoc()
# {
#     local log_files=/var/log/debesys/*cme*
#     if [ "$1" == "dev61" ]; then
#         log_files=~/dev61/var/log/debesys/*cme*
#     elif [ "$1" == "sim73" ]; then
#         log_files=~/sim73/var/log/debesys/*cme*
#     fi
#     emacs -nw `/bin/ls -tr $log_files | tail -1`
# }

# function rmoc()
# {
#     local log_files=/var/log/debesys/*cme*
#     if [ "$1" == "dev61" ]; then
#         log_files=~/dev61/var/log/debesys/*cme*
#     elif [ "$1" == "sim73" ]; then
#         log_files=~/sim73/var/log/debesys/*cme*
#     fi
#     rm -v `/bin/ls -tr $log_files | tail -1`
# }

# function lslu()
# {
#     local log_files=/var/log/debesys/*ledger_up-*
#     if [ "$1" == "dev61" ]; then
#         log_files=~/dev61/var/log/debesys/*ledger_up-*
#     fi
#     /bin/ls /bin/ls -tr $log_files | tail -1
# }

# function tllu()
# {
#     local log_files=/var/log/debesys/*ledger_up-*
#     if [ "$1" == "dev61" ]; then
#         log_files=~/dev61/var/log/debesys/*ledger_up-*
#     fi
#     /bin/ls -tr /bin/ls -tr $log_files | tail -1 | xargs tail -f
# }

# function edlu()
# {
#     local log_files=/var/log/debesys/*ledger_up-*
#     if [ "$1" == "dev61" ]; then
#         log_files=~/dev61/var/log/debesys/*ledger_up-*
#     fi
#     emacs -nw `/bin/ls -tr $log_files | tail -1`
# }

# function rmlu()
# {
#     local log_files=/var/log/debesys/*ledger_up-*
#     if [ "$1" == "dev61" ]; then
#         log_files=~/dev61/var/log/debesys/*ledger_up-*
#     fi
#     rm -v `/bin/ls -tr $log_files | tail -1`
# }

# function lslr()
# {
#     local log_files=/var/log/debesys/*ledger_req-*
#     if [ "$1" == "dev61" ]; then
#         log_files=~/dev61/var/log/debesys/*ledger_req-*
#     fi
#     /bin/ls /bin/ls -tr $log_files | tail -1
# }

# function tllr()
# {
#     local log_files=/var/log/debesys/*ledger_req-*
#     if [ "$1" == "dev61" ]; then
#         log_files=~/dev61/var/log/debesys/*ledger_req-*
#     fi
#     /bin/ls -tr /bin/ls -tr $log_files | tail -1 | xargs tail -f
# }

# function edlr()
# {
#     local log_files=/var/log/debesys/*ledger_req-*
#     if [ "$1" == "dev61" ]; then
#         log_files=~/dev61/var/log/debesys/*ledger_req-*
#     fi
#     emacs -nw `/bin/ls -tr $log_files | tail -1`
# }

# function rmlr()
# {
#     local log_files=/var/log/debesys/*ledger_req-*
#     if [ "$1" == "dev61" ]; then
#         log_files=~/dev61/var/log/debesys/*ledger_req-*
#     fi
#     rm -v `/bin/ls -tr $log_files | tail -1`
# }

# function lsbr()
# {
#     local log_files=/var/log/debesys/*bouncerd*
#     if [ "$1" == "dev61" ]; then
#         log_files=~/dev61/var/log/debesys/*bouncerd*
#     fi
#     /bin/ls /bin/ls -tr $log_files | tail -1
# }

# function tlbr()
# {
#     local log_files=/var/log/debesys/*bouncerd*
#     if [ "$1" == "dev61" ]; then
#         log_files=~/dev61/var/log/debesys/*bouncerd*
#     fi
#     /bin/ls -tr /bin/ls -tr $log_files | tail -1 | xargs tail -f
# }

# function edbr()
# {
#     local log_files=/var/log/debesys/*bouncerd*
#     if [ "$1" == "dev61" ]; then
#         log_files=~/dev61/var/log/debesys/*bouncerd*
#     fi
#     emacs -nw `/bin/ls -tr $log_files | tail -1`
# }

# function rmbr()
# {
#     local log_files=/var/log/debesys/*bouncerd*
#     if [ "$1" == "dev61" ]; then
#         log_files=~/dev61/var/log/debesys/*bouncerd*
#     fi
#     rm -v `/bin/ls -tr $log_files | tail -1`
# }

# function lses()
# {
#     local log_files=/var/log/debesys/*edgeserver*
#     if [ "$1" == "dev61" ]; then
#         log_files=~/dev61/var/log/debesys/*edgeserver*
#     fi
#     /bin/ls /bin/ls -tr $log_files | tail -1
# }

# function tles()
# {
#     local log_files=/var/log/debesys/*edgeserver*
#     if [ "$1" == "dev61" ]; then
#         log_files=~/dev61/var/log/debesys/*edgeserver*
#     fi
#     /bin/ls -tr /bin/ls -tr $log_files | tail -1 | xargs tail -f
# }

# function edes()
# {
#     local log_files=/var/log/debesys/*edgeserver*
#     if [ "$1" == "dev61" ]; then
#         log_files=~/dev61/var/log/debesys/*edgeserver*
#     fi
#     emacs -nw `/bin/ls -tr $log_files | tail -1`
# }

# function rmes()
# {
#     local log_files=/var/log/debesys/*edgeserver*
#     if [ "$1" == "dev61" ]; then
#         log_files=~/dev61/var/log/debesys/*edgeserver*
#     fi
#     rm -v `/bin/ls -tr $log_files | tail -1`
# }