# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions

GIT_PS1_SHOWDIRTYSTATE=true
. ~/githome/rhel/.git-prompt.sh
. ~/githome/rhel/.git-completion.sh

export PS1="\h\[\033[1;30m\]\$(__git_ps1) \[\033[0;0m\]\w \n>"
#             \[\033[1;34m\] Start color dark grey.
#                                        \[\033[0;0m\] Stop color.
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

alias edbrc='emacs -nw ~/githome/rhel/.bashrc'
alias ee='emacs -nw'
alias c='emacsclient -n'
alias ls='ls -aFC --color=always'
alias h='history'
alias hg='history | grep'
alias rw=~/githome/setxtitle.sh
unset PROMPT_COMMAND
alias m='make -Rr -j 8 -C `git rev-parse --show-toplevel`'
alias ff='find . -type d -path "*/build" -prune -o -path "*/.git" -prune -o -path "*/ext" -prune -o -path "*/pycommon" -prune -o \( \! -iname "*.ico" -and \! -iname "TAGS" -and \! -iname "FILES" -and \! -iname "BROWSE" -and \! -iname "*.cs" -and \! -iname "*.png" -and \! -iname "*.jar" -and \! -iname "*.pyc" -and \! -iname "*.o" -and \! -iname "*.d" \! -iname "*.a" \! -name "*.so" \! -iname "*.bin" \! -iname "*.sql" \! -iname "*.dat" \) -print0 | xargs -0 grep -iHn'
alias git-add-mod='git status | grep modified | cut -d " " -f 4 | xargs --max-args=1 git add -v'
alias glog='git glog | head'
alias galias='git config --list | grep alias'
alias tlsrh='/bin/ls -tr /var/lib/order-connector/*_send_recv_* | tail -1 | xargs tail -f | sed -u "s/\x01/  /g"'
alias tlsrhbb1='/bin/ls -tr ~/bb1/var/lib/order-connector/*_send_recv_* | tail -1 | xargs tail -f | sed -u "s/\x01/  /g"'
alias tlsr='/bin/ls -tr /var/lib/order-connector/*_send_recv_* | tail -1 | xargs tail -f | sed -u "s/\x01/  /g" | grep --line-buffered -v 35=0'
alias edsr='emacs -nw `/bin/ls -tr /var/lib/order-connector/*_send_recv_* | tail -1`'
alias tloc='/bin/ls -tr /var/log/*cme* | tail -1 | xargs tail -f'
alias tlocbb1='/bin/ls -tr ~/bb1/var/log/debesys/*cme* | tail -1 | xargs tail -f'
alias rmoc='/bin/ls -tr /var/log/*cme* | tail -1 | xargs rm'
alias edoc='emacs -nw `/bin/ls -tr /var/log/*cme* | tail -1`'
alias soc='kill `cat /var/run/cme.pid`'
alias oc?='cat /var/run/cme.pid; ps -ef | grep cme'
alias rmvol='rm /var/lib/order-connector/*'
alias pbin='pushd `git rev-parse --show-toplevel`/build/x86-64/debug/bin'
alias pext='pushd `git rev-parse --show-toplevel`/ext'
alias cdrt='cd `git rev-parse --show-toplevel`'
alias prt='pushd `git rev-parse --show-toplevel`'
alias dr="cd ~/dev-root"
alias edcfg='emacs -nw `git rev-parse --show-toplevel`/build/x86-64/debug/etc/debesys/cme_oc_config.xml'
alias run='`git rev-parse --show-toplevel`/run'
alias envs='echo PATH $PATH; echo LD_LIBRARY_PATH $LD_LIBRARY_PATH; echo C_INCLUDE_PATH $C_INCLUDE_PATH; echo CPLUS_INCLUDE_PATH $CPLUS_INCLUDE_PATH; echo PYTHONPATH $PYTHONPATH; echo PYTHONHOME $PYTHONHOME; echo SWIG_LIB $SWIG_LIB; echo DEBENV_ENGAGED $DEBENV_ENGAGED'
alias bb1='ssh root@10.202.0.61'
alias mbb1="sshfs root@10.202.0.61:/ ~/bb1"
alias ocperf="ssh root@192.168.254.180"
alias stperf="ssh root@192.168.254.187"
alias repo="python ~/githome/get-repo.py"

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

function cpcfg_()
{
    cp -v /home/debesys/configs/cme_oc_config.xml `git rev-parse --show-toplevel`/build/x86-64/debug/etc/debesys/cme_oc_config.xml;
    cp -v `git rev-parse --show-toplevel`/build/x86-64/debug/etc/debesys/lbm_config.xml `git rev-parse --show-toplevel`/build/x86-64/debug/etc/debesys/lbm_config.orig.xml;
    cp -v `git rev-parse --show-toplevel`/build/x86-64/debug/etc/debesys/lbm_config_backbone.xml `git rev-parse --show-toplevel`/build/x86-64/debug/etc/debesys/lbm_config_backbone.orig.xml;
}
alias cpcfg=cpcfg_

function bblbm_()
{
    rm -v `git rev-parse --show-toplevel`/build/x86-64/debug/etc/debesys/lbm_config.xml;
    cp -v `git rev-parse --show-toplevel`/build/x86-64/debug/etc/debesys/lbm_config_backbone.orig.xml `git rev-parse --show-toplevel`/build/x86-64/debug/etc/debesys/lbm_config.xml;
}
alias bblbm=bblbm_

function dvlbm_()
{
    rm -v `git rev-parse --show-toplevel`/build/x86-64/debug/etc/debesys/lbm_config.xml;
    cp -v `git rev-parse --show-toplevel`/build/x86-64/debug/etc/debesys/lbm_config.orig.xml `git rev-parse --show-toplevel`/build/x86-64/debug/etc/debesys/lbm_config.xml;
}
alias dvlbm=dvlbm_

if [ ! -f /var/log/profiles ]
then
    touch /var/log/profiles
    chmod a+rw /var/log/profiles
fi
echo .bashrc ran at $(date) >> /var/log/profiles
