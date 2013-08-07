# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions
ulimit -c unlimited

# This seems to really slow down the __git_ps1 function, sometimes up to 1 second,
# which is too long for the prompt to return.
# GIT_PS1_SHOWDIRTYSTATE=true
source ~/githome/rhel/.git-prompt.sh
source ~/githome/rhel/.git-completion.sh

if [ -f ~/amazon_keys.sh ]; then
    source ~/amazon_keys.sh
fi

export PS1="\h\[\033[1;30m\]\$(__git_ps1) \[\033[0;0m\]\w \n>"
#             \[\033[1;34m\] Start color dark grey.
#                                        \[\033[0;0m\] Stop color.
#. ~/githome/rhel/.git-prompt-alternate.sh
#export PS1="\h \[\033[1;30m\]\$(parse_git_branch) \[\033[0;0m\]\w \n>"

export EDITOR="emacs -nw"
# ALTERNATE_EDITOR causes emacs to be opened if emacsclient is invoked and no instance is running.
export ALTERNATE_EDITOR=emacs
# Don't quote environment variables with tilde.
export LBM_LICENSE_FILENAME=~/29WestLicense.txt

# Cause the dynamic linker to resolve all symbols at program startup.  Useful to ensure uncalled
# functions won't fail resolution at runtime.  I had to turn off this because the ringer development
# in scala caused an LBM symbol to not be recognized and failed to run.  To unset use:
# unset LD_BIND_NOW
# export LD_BIND_NOW=yes
export PATH=$PATH:/home/debesys/Downloads/meld-1.6.1/bin

. ~/githome/rhel/logs.sh

alias sb='source ~/.bashrc'
alias edbrc='emacs -nw ~/githome/rhel/.bashrc'
alias ee='emacs -nw'
alias c='emacsclient -n'
alias ls='ls -aFCh --color=always'
alias h='history'
alias hg='history | grep'
alias rw=~/githome/setxtitle.sh
unset PROMPT_COMMAND

# Use optimize-find.py to help decide which directories and extensions to filter.
#alias ff='find . -type d -path "*/build" -prune -o -path "*/.git" -prune -o -path "*/ext" -prune -o -path "*/pycommon" -prune -o \( \! -iname "*.ico" -and \! -iname "TAGS" -and \! -iname "FILES" -and \! -iname "BROWSE" -and \! -iname "*.cs" -and \! -iname "*.png" -and \! -iname "*.jar" -and \! -iname "*.pyc" -and \! -iname "*.o" -and \! -iname "*.d" -and \! -iname "*.a" -and \! -name "*.so" -and \! -iname "*.bin" -and \! -iname "*pdf" -and \! -iname "*.java"  -and \! -iname "*xml" -and \! -iname "*.scala" -and \! -iname "*png" -and \! -iname "*.txt" -and \! -iname "*.html" -and \! -iname "*.php" -and \! -iname "*.css" -and \! -iname "*.js" -and \! -iname "*.cs" -and \! -iname "*.json" -and \! -iname "*.sql" -and \! -iname "*.dat" \) -print0 | xargs -0 grep -iHn'

ff_dir=' -path "*/build" -prune -o '
ff_dir+=' -path "*/.git" -prune -o '
ff_dir+=' -path "*/ext" -prune -o '
ff_dir+=' -path "*/pycommon" -prune -o '
ff_file=' \! -name "TAGS" ' # NOTE: The first file does not have the '-and'.
ff_file+=' -and \! -name "FILES" '
ff_file+=' -and \! -name "BROWSE" '
ff_file+=' -and \! -iname "*.a" '
ff_file+=' -and \! -iname "*.bin" '
ff_file+=' -and \! -iname "*.cs" '
ff_file+=' -and \! -iname "*.css" '
ff_file+=' -and \! -iname "*.d" '
ff_file+=' -and \! -iname "*.dat" '
ff_file+=' -and \! -iname "*.html" '
ff_file+=' -and \! -iname "*.ico" '
ff_file+=' -and \! -iname "*.jar" '
ff_file+=' -and \! -iname "*.js" '
ff_file+=' -and \! -iname "*.json" '
ff_file+=' -and \! -iname "*.o" '
ff_file+=' -and \! -iname "*.pdf" '
ff_file+=' -and \! -iname "*.php" '
ff_file+=' -and \! -iname "*.png" '
ff_file+=' -and \! -iname "*.pyc" '
ff_file+=' -and \! -iname "*.so" '
ff_file+=' -and \! -iname "*.sql" '
ff_file+=' -and \! -iname "*.txt" '
ff_file+=' -and \! -iname "*.xml" '
alias ff="find . -type d $ff_dir \( $ff_file \) -print0 | xargs -0 grep -iHn"

alias git-add-mod='git status | grep modified | cut -d " " -f 4 | xargs --max-args=1 git add -v'
alias glog='git glog | head -n 15'
alias galias='git config --list | grep alias'
alias soc='kill `cat /var/run/cme.pid`'
alias oc?='cat /var/run/cme.pid; ps -ef | grep cme | grep -v grep'
alias emacs?='ps -ef | grep emacs | grep -v "grep emacs" | grep -v "emacs -nw"'
alias rmvol='rm /var/lib/order-connector/*'
alias pbin='pushd `git rev-parse --show-toplevel`/build/x86-64/debug/bin'
alias pext='pushd `git rev-parse --show-toplevel`/ext'
alias cdrt='cd `git rev-parse --show-toplevel`'
alias prt='pushd `git rev-parse --show-toplevel`'
alias dr="cd ~/dev-root"
alias edcfg='emacs -nw /etc/debesys/cme_oc_config.conf'
alias run='`git rev-parse --show-toplevel`/run'
alias lszk='`git rev-parse --show-toplevel`/run python `git rev-parse --show-toplevel`/darwin/dashboard/lszk.py'
alias rmzk='`git rev-parse --show-toplevel`/run python `git rev-parse --show-toplevel`/darwin/dashboard/rmzk.py'
alias oczk='lszk /srv/alive/oc -r; lszk /srv/oc -r | xargs --delimiter="\n" -n 1 echo "     "'
alias envs='echo PATH $PATH; echo LD_LIBRARY_PATH $LD_LIBRARY_PATH; echo C_INCLUDE_PATH $C_INCLUDE_PATH; echo CPLUS_INCLUDE_PATH $CPLUS_INCLUDE_PATH; echo PYTHONPATH $PYTHONPATH; echo PYTHONHOME $PYTHONHOME; echo SWIG_LIB $SWIG_LIB; echo DEBENV_ENGAGED $DEBENV_ENGAGED'
alias dev61='ssh root@10.202.0.61'
alias sim73='ssh root@10.202.0.73'
alias sim81='ssh root@10.202.0.81'
alias prod54='ssh root@10.202.0.54'
alias mdev61="sshfs root@10.202.0.61:/ ~/dev61"
alias msim73="sshfs root@10.202.0.73:/ ~/sim73"
alias msim81="sshfs root@10.202.0.81:/ ~/sim81"
alias mprod54="sshfs root@10.202.0.54:/ ~/prod54"
alias m180='sshfs root@192.168.254.180:/ ~/180'
alias ocperf="ssh root@192.168.254.180"
alias m187='sshfs root@192.168.254.187:/ ~/187'
alias stperf="ssh root@192.168.254.187"
alias repo="python ~/githome/get-repo.py"
alias mdbd='sudo mount -o user=intad/tweiss -t cifs //chifs01.int.tt.local/Share/Dead_By_Dawn /mnt/dbd/'
alias cli_mt='run `git rev-parse --show-toplevel`/ext/linux/x86-64/release/bin/cli_mt 10.203.0.43:2181'
alias jtrader="/usr/java/jdk1.7.0_03/bin/java -cp JTrader.jar JTrader &"
alias ttr='`git rev-parse --show-toplevel`/run python `git rev-parse --show-toplevel`/t_trader/tt/ttrader/t_trader.py --disable-ledger'
alias grp="git rev-parse --short"
alias chrome="/opt/google/chrome/google-chrome --enable-plugins &"

# To view the definition of a function, do 'type <function>'.
function cf() { emacsclient -n `find . -name $1`; }
function f() { find . -name $1 -print; }
function rmbranch()
{
    echo "git push origin --delete $1";
    git push origin --delete $1;
    echo "git branch -d $1";
    git branch -d $1;
}

function koc_()
{
    ps -ef | grep cme;
    ps -ef | grep cme | grep -v grep | cut -d " " -f 3 | xargs kill -9;
    if [ -f /var/run/cme.pid ]; then rm -v /var/run/cme.pid; fi;
}
alias koc=koc_

function git-sync_()
{
    echo "pushd `git rev-parse --show-toplevel`";
    pushd `git rev-parse --show-toplevel`;
    echo "git checkout master";
    git checkout master;
    echo "git pull";
    git pull;
    echo "git submodule update";
    git submodule update;
    echo "popd";
    popd;
}
alias git-sync=git-sync_

function cpcfg_()
{
    cp -v `git rev-parse --show-toplevel`/build/x86-64/debug/etc/debesys/cme_oc_config.conf /etc/debesys/cme_oc_config.LATEST.conf;
    cp -v `git rev-parse --show-toplevel`/build/x86-64/debug/etc/debesys/lbm_config.xml /etc/debesys/lbm.conf;
    cp -v `git rev-parse --show-toplevel`/build/x86-64/debug/etc/debesys/lbm_config.xml /etc/debesys/lbm.local.conf;
    #cp -v `git rev-parse --show-toplevel`/build/x86-64/debug/etc/debesys/lbm_config_backbone.xml /etc/debesys/lbm.backbone.conf;
    cp -v ~/dev61/etc/debesys/lbm.conf /etc/debesys/lbm.dev.conf
    cp -v ~/sim73/etc/debesys/lbm.conf /etc/debesys/lbm.sim.conf
}
alias cpcfg=cpcfg_

function devlbm_()
{
    rm -v /etc/debesys/lbm.conf;
    cp -v /etc/debesys/lbm.dev.conf /etc/debesys/lbm.conf;
    cp -v /etc/debesys/lbm.dev.conf /etc/debesys/lbm_config.xml; # T Trader
}
alias devlbm=devlbm_

function locallbm_()
{
    rm -v /etc/debesys/lbm.conf;
    cp -v /etc/debesys/lbm.local.conf /etc/debesys/lbm.conf;
    cp -v /etc/debesys/lbm.local.conf /etc/debesys/lbm_config.xml; # T Trader
}
alias locallbm=locallbm_

function simlbm_()
{
    rm -v /etc/debesys/lbm.conf;
    cp -v /etc/debesys/lbm.sim.conf /etc/debesys/lbm.conf;
    cp -v /etc/debesys/lbm.sim.conf /etc/debesys/lbm_config.xml; # T Trader
}
alias simlbm=simlbm_

function m_()
{
    # The $@ variable contains all the arguments.
    make -Rr -j 8 -C `git rev-parse --show-toplevel` "$@"
    if [ $? == 0 ]; then
        echo COMPILE SUCCESSFUL!
    else
        echo COMPILE FAILED!
    fi
}
alias m=m_

function cleanvm_()
{
    ssh -i ~/.ssh/aws.pem root@10.203.0.136 "rm -rfv /etc/debesys";
    ssh -i ~/.ssh/aws.pem root@10.203.0.136 "rm -rfv /opt/debesys";
    ssh -i ~/.ssh/aws.pem root@10.203.0.136 "rm -v /etc/init.d/cme";
}
alias cleanvm=cleanvm_

function em_()
{
    isemacs=`emacs?`
    if [[ -z $isemacs ]]; then
        echo emacs is not running, starting emacs $@;
        emacs "$@" &
    else
        echo emacs is running, sending $@
        echo $isemacs;
        emacsclient -n "$@"
    fi
}
alias em=em_

function gdb_()
{
    if [ -f ~/.gdbinit ]; then
        echo ~/.gdbinit exists, likely from emacs;
    fi
    gdb
}
alias gdb=gdb_

function mkchefec2()
{
    if [ -z "$1" ]; then
        echo Usage: you must pass the node name.
        return
    fi

    # rhel 6.4 ami-7d0c6314

    echo ./run python deploy/chef/scripts/ec2_server.py --size m1.medium --ami ami-7d0c6314 --manager "Tom Weiss" --user ec2-user --environment dev -a $1
    ./run python deploy/chef/scripts/ec2_server.py --size m1.medium --ami ami-7d0c6314 --manager "Tom Weiss" --user ec2-user --environment dev -a $1

    local ip=`knife node show $1 | grep IP | tr -s ' ' | cut -d" " -f 2`
    if [ -z ip ]; then
        echo "Not able to find IP address of AWS instance."
        return
    fi

    # In order to run the sshfs command with the user root, we need to replace root's
    # authorized_keys with ec2-user's authorized_keys.  The root use does not otherwise
    # allow for mounting the file system with write permission.
    echo ssh -t ec2-user@$ip -i ~/.ssh/aws.pem "sudo cp /root/.ssh/authorized_keys /root/.ssh/authorized_keys_orig"
    ssh -t ec2-user@$ip -i ~/.ssh/aws.pem "sudo cp /root/.ssh/authorized_keys /root/.ssh/authorized_keys_orig"
    echo ssh -t ec2-user@$ip -i ~/.ssh/aws.pem "sudo cp /home/ec2-user/.ssh/authorized_keys /root/.ssh/authorized_keys"
    ssh -t ec2-user@$ip -i ~/.ssh/aws.pem "sudo cp /home/ec2-user/.ssh/authorized_keys /root/.ssh/authorized_keys"

    mkdir -pv ~/$1
    echo sshfs root@$ip:/ ~/$1 -o IdentityFile=~/.ssh/aws.pem
    sshfs root@$ip:/ ~/$1 -o IdentityFile=~/.ssh/aws.pem
}

function rmchefec2()
{
    if [ -z "$1" ]; then
        echo Usage: you must pass the node name.
        return
    fi

    echo ./run python deploy/chef/scripts/ec2_server.py -d $1
    ./run python deploy/chef/scripts/ec2_server.py -d $1

    echo sudo umount ~/$1
    sudo umount ~/$1

    echo rmdir ~/$1
    rmdir ~/$1
}


if [ ! -f /var/log/profiles ]
then
    if [ -w /var/log/profiles ]; then
        touch /var/log/profiles
        chmod a+rw /var/log/profiles
    fi
fi

if [ -w /var/log/profiles ]; then
    echo .bashrc ran at $(date) >> /var/log/profiles
fi
