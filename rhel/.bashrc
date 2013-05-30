# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions

# This seems to really slow down the __git_ps1 function, sometimes up to 1 second,
# which is too long for the prompt to return.
# GIT_PS1_SHOWDIRTYSTATE=true
source ~/githome/rhel/.git-prompt.sh
source ~/githome/rhel/.git-completion.sh

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
# functions won't fail resolution at runtime.
export LD_BIND_NOW=yes
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
alias m='make -Rr -j 8 -C `git rev-parse --show-toplevel`'
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
alias bb1='ssh root@10.202.0.61'
alias mbb1="sshfs root@10.202.0.61:/ ~/bb1"
alias m180='sshfs root@192.168.254.180:/ ~/180'
alias ocperf="ssh root@192.168.254.180"
alias m187='sshfs root@192.168.254.187:/ ~/187'
alias stperf="ssh root@192.168.254.187"
alias repo="python ~/githome/get-repo.py"
alias dbd='sudo mount -o user=intad/tweiss -t cifs //chifs01.int.tt.local/Share /mnt/dbd/'
alias cli_mt='run `git rev-parse --show-toplevel`/ext/linux/x86-64/release/bin/cli_mt 10.203.0.43:2181'
alias jtrader="/usr/java/jdk1.7.0_03/bin/java -cp JTrader.jar JTrader &"
alias ttr="`git rev-parse --show-toplevel`/run python t_trader/tt/ttrader/t_trader.py"
alias grp="git rev-parse --short"

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
    cp -v `git rev-parse --show-toplevel`/build/x86-64/debug/etc/debesys/lbm_config.xml /etc/debesys/lbm_config.xml;
    cp -v `git rev-parse --show-toplevel`/build/x86-64/debug/etc/debesys/lbm_config.xml /etc/debesys/lbm_config.orig.xml;
    cp -v `git rev-parse --show-toplevel`/build/x86-64/debug/etc/debesys/lbm_config_backbone.xml /etc/debesys/lbm_config_backbone.orig.xml;
}
alias cpcfg=cpcfg_

function bblbm_()
{
    rm -v /etc/debesys/lbm_config.xml;
    cp -v /etc/debesys/lbm_config_backbone.orig.xml /etc/debesys/lbm_config.xml;
}
alias bblbm=bblbm_

function dvlbm_()
{
    rm -v /etc/debesys/lbm_config.xml;
    cp -v /etc/debesys/lbm_config.orig.xml /etc/debesys/lbm_config.xml;
}
alias dvlbm=dvlbm_

if [ ! -f /var/log/profiles ]
then
    touch /var/log/profiles
    chmod a+rw /var/log/profiles
fi
echo .bashrc ran at $(date) >> /var/log/profiles
