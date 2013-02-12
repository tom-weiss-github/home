# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions



export PS1="\h \w \n>"
export EDITOR="emacs -nw"
# ALTERNATE_EDITOR causes emacs to be opened if emacsclient is invoked and no instance is running.
export ALTERNATE_EDITOR=emacs
# Don't quote environment variables with tilde.
export LBM_LICENSE_FILENAME=~/29WestLicense.txt
# Cause the dynamic linker to resolve all symbols at program startup.  Useful to ensure uncalled
# functions won't fail resolution at runtime.
export LD_BIND_NOW=yes
export PATH=$PATH:/home/debesys/Downloads/meld-1.6.1/bin


alias ee='emacs -nw'
alias c='emacsclient -n'
alias ls='ls -aFC --color=always'
alias h='history'
alias hg='history | grep'
alias rw=~/githome/setxtitle.sh
unset PROMPT_COMMAND
alias m='make -Rr -j 8 -C `git rev-parse --show-toplevel`'
alias ff='find . -type d -path "*/build" -prune -o -path "*/.git" -prune -o -path "*/ext" -prune -o -path "*/pycommon" -prune -o \( \! -iname "*.ico" -and \! -iname "TAGS" -and \! -iname "emacs-file-cache" -and \! -iname "*.cs" -and \! -iname "*.png" -and \! -iname "*.jar" -and \! -iname "*.pyc" -and \! -iname "*.o" -and \! -iname "*.d" \! -iname "*.a" \! -name "*.so" \! -iname "*.bin" \! -iname "*.sql" \! -iname "*.dat" \) -print0 | xargs -0 grep -iHn'
alias git-add-mod='git status | grep modified | cut -d " " -f 4 | xargs --max-args=1 git add -v'
alias g='git'
alias tlsrh='/bin/ls -tr /volatile/logs/*_send_recv_* | tail -1 | xargs tail -f | sed -u "s/\x01/  /g"'
alias tlsr='/bin/ls -tr /volatile/logs/*_send_recv_* | tail -1 | xargs tail -f | sed -u "s/\x01/  /g" | grep --line-buffered -v 35=0'
alias tloc='/bin/ls -tr /var/log/*cme* | tail -1 | xargs tail -f'
alias rmoc='/bin/ls -tr /var/log/*cme* | tail -1 | xargs rm'
alias edoc='emacs -nw `/bin/ls -tr /var/log/*cme* | tail -1`'
alias koc='ps -ef | grep cme | grep -v gdb | grep -v grep | grep -v tail | cut -d " " -f 3 | xargs -n 1 kill -9'
alias rmvol='rm /volatile/logs/*'
alias pbin='pushd `git rev-parse --show-toplevel`/build/x86-64/debug/bin'
alias pext='pushd `git rev-parse --show-toplevel`/ext'
alias prt='pushd `git rev-parse --show-toplevel`'
alias edcfg='emacs -nw `git rev-parse --show-toplevel`/build/x86-64/debug/etc/debesys/debesys_system_config.cfg'
alias run='`git rev-parse --show-toplevel`/run'
alias envs='echo PATH $PATH; echo LD_LIBRARY_PATH $LD_LIBRARY_PATH; echo C_INCLUDE_PATH $C_INCLUDE_PATH; echo CPLUS_INCLUDE_PATH $CPLUS_INCLUDE_PATH; echo PYTHONPATH $PYTHONPATH; echo PYTHONHOME $PYTHONHOME; echo SWIG_LIB $SWIG_LIB; echo DEBENV_ENGAGED $DEBENV_ENGAGED'


# To view the definition of a function, do 'type <function>'.
function cf() { emacsclient -n `find . -name $1`; }
function f() { find . -name $1 -print; }


if [ ! -f /var/log/profiles ]
then
    touch /var/log/profiles
    chmod a+rw /var/log/profiles
fi
echo .bashrc ran at $(date) >> /var/log/profiles
