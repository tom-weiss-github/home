# .bashrc
# Author: Tom Weiss
#

# Bash Notes
# !$ last argument
# ![string] run last command which contained string
# echo ![string] print last command which contained string

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions
# ulimit -c unlimited

# This seems to really slow down the __git_ps1 function, sometimes up to 1 second,
# which is too long for the prompt to return.
# GIT_PS1_SHOWDIRTYSTATE=true
source ~/githome/rhel/.git-prompt.sh
source ~/githome/rhel/.git-completion.sh

if [ -f ~/amazon_keys.sh ]; then
    source ~/amazon_keys.sh
fi

function set_branch()
{
    local branch=`__git_ps1`
    # Parenthesis are added to the beginning and end of the branch, remove them.
    branch=$(echo -n $branch | sed s/\(//g | sed s/\)//g)
    # echo $branch
    export b=$branch
}
export PROMPT_COMMAND=set_branch
# Now I can do "git push origin $b" and $b will always be the current branch.
# Don't forget to 'unset PROMPT_COMMAND' otherwise the default seems to rename
# the terminal based on the CWD.

export PS1="\h\[\033[0;33m\]\$(__git_ps1) \[\033[0;0m\]\w \n>"
#             \[\033[1;33m\] Start color brown.
#                                        \[\033[0;0m\] Stop color.
#. ~/githome/rhel/.git-prompt-alternate.sh
#export PS1="\h \[\033[1;30m\]\$(parse_git_branch) \[\033[0;0m\]\w \n>"

# Ubuntu stuff to make tig completion fast.
if [[ -f ~/.bash_completion.d/tig-completion.bash ]]; then
    # This file came from compiling tig from source.
    source ~/.bash_completion.d/tig-completion.bash
fi

if [[ -f /usr/share/bash-completion/completions/git ]]; then
    # This file was already present in Ubuntu, but I copied a slightly newer one from Git's git
    # repository to ensure a symbol was defined that the updated tig bash completion needed.
    source /usr/share/bash-completion/completions/git
fi

if [[ $HOSTNAME == jchi* ]]; then
    export NPM_CONFIG_PREFIX=~/.npm-global
    export PATH=~/.npm-global/bin:$PATH

    PROXY_URL="proxy-jch-ext-prod-coreinfra.trade.tt"
    PROXY_PORT="3128"
    export http_proxy="http://$PROXY_URL:$PROXY_PORT"
    export https_proxy="http://$PROXY_URL:$PROXY_PORT"
    export HTTP_PROXY="http://$PROXY_URL:$PROXY_PORT"
    export HTTPS_PROXY="http://$PROXY_URL:$PROXY_PORT"
    export ALL_PROXY="http://$PROXY_URL:$PROXY_PORT"
fi

# History across terminal sessions.
export HISTSIZE=20000
export HISTCONTROL=ignorespace
shopt -s histappend
# history -a => append current session's history to history file (happens at session exit)
# history -c => clear the current session's history
# history -r => reload the history from the history file into the current session
# Commenting this out because it seems that my terminal history has the wrong numbers so I can't !
# number anymore.  PROMPT_COMMAND="history -a;history -c;history -r;$PROMPT_COMMAND"
export HISTTIMEFORMAT='%F %T '
alias often='cat $HISTFILE | grep -v "#1" | sort | uniq -c'

if [[ -f ~/bin/emacs-24.3 ]]; then
    export myemacs=~/bin/emacs-24.3
    export myemacsclient=~/bin/emacsclient
elif [[ -f /usr/local/bin/emacs ]]; then
    export myemacs=/usr/local/bin/emacs
    export myemacsclient=/usr/local/bin/emacsclient
else
    export myemacs=/usr/bin/emacs
    export myemacsclient=/usr/bin/emacsclient
fi
# export myemacs=emacs
# export myemacsclient=emacsclient



export EDITOR="$myemacs -nw"
# ALTERNATE_EDITOR causes emacs to be opened if emacsclient is invoked and no instance is running.
export ALTERNATE_EDITOR=$myemacs
# Don't quote environment variables with tilde.
export LBM_LICENSE_FILENAME=~/29WestLicense.txt

# Exports for ec2_server.py.
export MANAGER="Tom Weiss"
export DEPT=""

# Cause the dynamic linker to resolve all symbols at program startup.  Useful to ensure uncalled
# functions won't fail resolution at runtime.  I had to turn off this because the ringer development
# in scala caused an LBM symbol to not be recognized and failed to run.  To unset use:
# unset LD_BIND_NOW
# export LD_BIND_NOW=yes
export CHGCMD_SET_STATE="false"
export CHGCMD_DESC_WIDTH=80
# export CHGCMD_FROM_SN=0
export DEVWS_SKIP_VALIDATE_REQUIREMENTS=1
export GIT_TT_BRANCH_IN_COMMIT_MSG=true
export UPLOAD_RC_VERSION_HERE=/home/tweiss/dev-root/__deploy_alternate
export PATH=$PATH:~/Downloads/meld-1.6.1/bin
export PATH=$PATH:/opt/redis/redis-2.8.17/src
export PATH=$PATH:/opt/scala-2.9.3/bin/
# export PATH=$JAVA_HOME/bin:$PATH
export INTAD_USER=tweiss
export INTAD_SSH_KEY=~/.ssh/id_rsa
export BCV_ENABLE_LDAP=1
export VCD_ORG=Dev_General
export JENKINS_USER=tweiss
export TTID_EMAIL=tom.weiss+--ttsa@tradingtechnologies.com
export TT_EMAIL=tom.weiss@tradingtechnologies.com
if [ -f ~/jenkins_token ]; then
    export JENKINS_TOKEN=$(head -n 1 ~/jenkins_token)
fi
if [ -f ~/github_token ]; then
    export GITHUB_TOKEN=$(head -n 1 ~/github_token)
fi
export PDS_REPO_ROOT=~/dev-root/pds
export LSS_SERVER_REPO_ROOT=~/dev-root/lss_server
export DEPLOYMENT_SCRIPTS_REPO_ROOT=~/dev-root/scripts
if [ -f $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/bashrc/chef.bash ]; then
    source $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/bashrc/chef.bash
    alias tth=ssh_by_hostname
fi
# export BUMP_COOKBOOK_VERSION_ALTERNATE_REPO=~/dev-root/cookbooks
# export USE_PYTHON3=1
export CHEF_LICENSE=accept-silent
export REQUEST_BUILD_SUPPRESS_TIPS=1
export DEPLOY_ONE_OFF_HIDE_EXPIRE_MSG=1
export DEPLOY_ONE_OFF_NO_WARN_HIDE_DAILY=1
export BUMP_COOKBOOK_VERSION_AUTO_EXECUTE=1
export BUMP_COOKBOOK_VERSION_ALLOW_MULTIBUMP=1
export KNIFE_SSH_ENABLE_INTERNAL_POOL=1
export KNIFE_SSH_REBOOT=1
# export KNIFE_SSH_UPGRADE_CHEF=16.10.8
export KNIFE_SSH_COMMAND_SEVERITY=debug
export KNIFE_SSH_ROLLING_UPGRADE=1
export FEATURE_TEST_EMAIL=tom.weiss@tradingtechnologies.com
export FEATURE_TEST_COMPANY="Deployment Team"
export FEATURE_TEST_USER=tweiss
# To run ringer:
# cp ringer.conf/srl_config_ringer.xml from some machine in int-dev-sim
# cp deploy/chef/cookbooks/srlabs/files/default/smds.lic /etc/debesys/
# export JAVA_HOME=/usr/java/jdk1.7.0_51
# export JRE_HOME=$JAVA_HOME/jre
export JAVA_HOME=/usr/java/jdk1.8.0_181
export JRE_HOME=/usr/java/jdk1.8.0_181/jre
export JAVA_8_181_HOME=/usr/java/jdk1.8.0_181
export JAVA_8_265_HOME=/usr/java/jdk1.8.0_265/openjdk-8u265-b01
# run /usr/java/jdk1.7.0_17/bin/java -Dversion="0.0.0" -cp ./ringer/target/Ringer.jar Ringer --srl-config /etc/debesys/srl_config_ringer.xml -v -o
export MY_ONE_OFF_VERSION=0.88.88
export ENABLE_POST_TO_SERVICENOW=1
export BUMP_COOKBOOK_EMAIL_ON_PYTHON_PACKAGE_EXCEPTION=1
export PYTHONWARNINGS="ignore"

alias exc="knife search node \"chef_environment:ext-prod-live AND recipe:exchange_compliance*\" -a run_list --config ~/.chef/knife.external.rb"
alias nocolor="sed 's/\x1b\[[0-9;]*m//g'"
alias ssh?='ss | grep ssh'
alias ireboot="t3 int-dev-cert server_actions post_reboot --name "
alias nojava="rm -rf client.java && rm -rf .git/modules/client.java"
alias xclip='xclip -sel clip'
alias off='sudo shutdown -P now'
alias hibernate='echo sudo systemctl hibernate is broken'
alias hdmi1='sudo ddccontrol -r 0x60 -w 17 dev:/dev/i2c-4 > /dev/null 2>&1'
alias suspend='sudo systemctl suspend'
alias lock='gnome-screensaver-command -l'
alias wifiscan='nmcli device wifi rescan'
alias wifils='nmcli device wifi list'
alias wks6='ssh -X 10.195.2.49'
alias wks='ssh -X 10.195.1.89'
alias todo='emacs -nw ~/todo.txt'
alias rooms='cat ~/githome/rooms.txt'
alias sb='source ~/.bashrc'
alias edbrc="$myemacs -nw ~/githome/rhel/.bashrc"
alias ee="$myemacs -nw"
alias c="$myemacsclient -n"
alias ls='ls -aFCh --color=always'
alias h='history | tail -n 50'
alias hg='history | grep'
alias rwbr='~/githome/setxtitle.sh $(__git_ps1)'
alias vm16='ssh tweiss@10.202.0.16 -i ~/.ssh/id_rsa'
alias clk='python ~/githome/world_time.py'
alias gdb='gdb -n'
alias gt='gnome-terminal &'
alias push='echo git push origin $b; git push origin $b'
alias swarm="/opt/virtualenv/devws/bin/python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/swarm.py --verbose "
alias rr='cd `git rev-parse --show-toplevel`'
# alias vc="$DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/view_changes.py "
export TEMP_VM_CHEF_ENV=int-dev-cert
export TEMP_VM_CPU=4
export TEMP_VM_MEMORY=8
# export TEMP_VM_CHEF_TAG='basegofast'
# alias tempvm="$DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/temp_vm.py -v --log-dir /var/log/debesys "
# alias nutanix="$DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/nutanix_server.py -ov "
# alias bcv="$DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/bump_cookbook.py"
alias nochef="/opt/virtualenv/devws/bin/python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/disable_chef.py"
# alias upenv="$DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/update_environment.py"
# alias ec2="$DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/ec2_instance.py -vo --route53 "
# alias mergetest="$DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/check_repo.py"
# alias fta="$DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/feature_test_assistant.py"
# alias cof="$DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/deploy_one_off.py"
# alias decom="$DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/request_prod_decom.py"
# alias vlan="$DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/request_vlan.py"
alias smile='rename_terminal_title ":-)"'
alias git-commit-hook="cp ~/githome/prepare-commit-msg .git/hooks/; chmod a+x .git/hooks/prepare-commit-msg"
# alias tdeploy='history -s "/opt/virtualenv/devws/bin/python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/knife_ssh.py --knife-config ~/.chef/knife.external.rb --audit-runlist --concurrency 50 -a -e environments -q query -c cookbooks -r CHG123456 --test-run"; echo Test run command inserted into history, use up arrow to edit.'
# alias hotfixer='$DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/hotfixer.py --prod --uat'
# alias edeploy='\^--test-run\^--send-summary-email\^'
alias tkw="tmux kill-window"
alias tkp="tmux kill-pane"
alias tsud="tmux split-window"
alias tnw="tmux new-window"
alias tks="tmux kill-server"
alias tcp="tmux show-buffer -b 0 | xclip -i"
alias fakechef="cp -v ~/.chef/knife.training.rb.orig ~/.chef/knife.rb && cp -v ~/.chef/knife.training.rb.orig ~/.chef/knife.external.rb && cp -v ~/.chef/knife.training.rb.orig ~/.chef/knife.ttsdk.rb && export BUMP_COOKBOOK_VERSION_NO_NOTES=1 && export BUMP_COOKBOOK_VERSION_NO_KNIFE_CHECK=1 && echo BUMP_COOKBOOK_VERSION_NO_NOTES/BUMP_COOKBOOK_VERSION_NO_KNIFE_CHECK have been set."
alias realchef="cp -v ~/.chef/knife.rb.orig ~/.chef/knife.rb && cp -v ~/.chef/knife.external.rb.orig ~/.chef/knife.external.rb && cp -v ~/.chef/knife.ttsdk.rb.orig ~/.chef/knife.ttsdk.rb && unset BUMP_COOKBOOK_VERSION_NO_NOTES && unset BUMP_COOKBOOK_VERSION_NO_KNIFE_CHECK && echo BUMP_COOKBOOK_VERSION_NO_NOTES/BUMP_COOKBOOK_VERSION_NO_KNIFE_CHECK have been unset."
alias awsauthexcomp="/opt/virtualenv/devws3/bin/python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/aws_authenticator.py --account prod --role exchange-compliance --env"
alias upintenv='pushd deploy/chef/environments; for env_file in int-dev*.rb; do knife environment from file $env_file --config ~/.chef/knife.rb; done; popd'
alias brm='git tt br m'
alias aec='source /opt/virtualenv/exchange_compliance/bin/activate && source orders/cf/audit/pythonpath.sh'
# alias apython='source `git rev-parse --show-toplevel`/orders/compliance/cf/pythonpath.sh; /opt/virtualenv/exchange_compliance_2_7_14/bin/python'
alias apython='source `git rev-parse --show-toplevel`/orders/compliance/cf/pythonpath.sh; /opt/virtualenv/exchange_compliance_3_9_13/bin/python'
alias dpy=/opt/virtualenv/devws/bin/python
alias dpy3=/opt/virtualenv/devws3/bin/python
alias cdr='cat `ls -d1t ~/deployment_receipts/* | head -n 1` | xclip -i'
# alias esetrcv='eknife exec $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/snacks/set_rc_version.rb'
alias nutanix_cpu='knife ssh "(chef_environment:int-dev* OR chef_environment:int-stage* OR chef_environment:int-sqe*) AND (NOT chef_environment:int-dev-jenkins) (NOT chef_environment:*perf*) AND name:*vm* AND (NOT creation_info_machine_origin:temp_hive)" "uptime" -a ipaddress --concurrency 20 | grep -v "load average: 0."'
alias kcu="knife cookbook upload --config ~/.chef/knife.rb --cookbook-path=deploy/chef/cookbooks "
alias ekcu="knife cookbook upload --config ~/.chef/knife.external.rb --cookbook-path=deploy/chef/cookbooks"

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
ff_file+=' -and \! -iname "*.o" '
ff_file+=' -and \! -iname "*.pdf" '
ff_file+=' -and \! -iname "*.php" '
ff_file+=' -and \! -iname "*.png" '
ff_file+=' -and \! -iname "*.pyc" '
ff_file+=' -and \! -iname "*.so" '
ff_file+=' -and \! -iname "*.sql" '
ff_file+=' -and \! -iname "*.txt" '
ff_file+=' -and \! -iname "*.xml" '
alias ff="find . -type d $ff_dir \( $ff_file \) -print0 | xargs -0 grep -iHns"

alias git-add-mod='git status | grep modified | cut -d " " -f 4 | xargs --max-args=1 git add -v'
alias allbranches="git for-each-ref --format='%(committerdate) %09 %(authorname) %09 %(refname)' | sort -k5n -k2M -k3n -k4n"
alias glog='git --no-pager glog -10'
alias galias='git config --list | grep alias'
alias soc='kill `cat /var/run/cme.pid`'
alias oc?='cat /var/run/cme.pid; ps -ef | grep cme | grep -v grep'
# Why 'grep -v ' -nw '?  I want to find if Emacs is running in non-terminal mode, I don't care about
# terminal mode instances.
alias emacs?='ps -ef | grep emacs | grep -v "grep" | grep -v " -nw "'
alias rmvol='rm /var/lib/order-connector/*'
alias pbin='pushd `git rev-parse --show-toplevel`/build/x86-64/debug/bin'
alias pext='pushd `git rev-parse --show-toplevel`/ext'
alias cdrt='cd `git rev-parse --show-toplevel`'
alias prt='pushd `git rev-parse --show-toplevel`'
alias default="cd ~/dev-root/default"
alias alternate="cd ~/dev-root/alternate"
alias another="cd ~/dev-root/another"
alias edcfg="$myemacs -nw /etc/debesys/cme_oc_config.conf"
#alias run='`git rev-parse --show-toplevel`/run'
alias dpython=/opt/virtualenv/devws/bin/python2
alias ttknife='/opt/virtualenv/devws/bin/python `git rev-parse --show-toplevel`/ttknife'
alias envvers='knife environment list | xargs -n 1 -i knife environment show \{\} -a cookbook_versions'
#alias lszk='`git rev-parse --show-toplevel`/run python `git rev-parse --show-toplevel`/darwin/python/lszk'
#alias rmzk='`git rev-parse --show-toplevel`/run python `git rev-parse --show-toplevel`/darwin/python/rmzk'
alias oczk='lszk /srv/alive/oc -r; lszk /srv/oc -r | xargs --delimiter="\n" -n 1 echo "     "'
alias envs='echo PATH $PATH; echo LD_LIBRARY_PATH $LD_LIBRARY_PATH; echo C_INCLUDE_PATH $C_INCLUDE_PATH; echo CPLUS_INCLUDE_PATH $CPLUS_INCLUDE_PATH; echo PYTHONPATH $PYTHONPATH; echo PYTHONHOME $PYTHONHOME; echo SWIG_LIB $SWIG_LIB; echo DEBENV_ENGAGED $DEBENV_ENGAGED'

alias repo="python ~/githome/get-repo.py"
if [ -e /etc/centos-release ]; then
    alias mdbd='sudo mount -o user=intad/tweiss -t cifs //chifs01.int.tt.local/Share/Dead_By_Dawn /mnt/dbd/'
else
    alias mdbd='sudo mount.cifs -o user=tweiss,vers=1.0 //chifs01.int.tt.local/Share/Dead_By_Dawn /mnt/dbd'
fi
alias kolmar='sudo mount.cifs -o user=weiss //192.168.0.3/Users /mnt/kolmar'
alias gld2vm49='sudo mount.cifs -o user=tweiss,uid=1000,gid=1000 //10.195.2.49/shared /home/tweiss/gld2vm49'
alias gld1vm89='sudo mount.cifs -o user=tweiss,uid=1000,gid=1000 //10.195.1.89/shared /home/tweiss/gld1vm89'
alias jch='sudo mount.cifs -o user=tweiss //chijchfs01/share/replication /home/tweiss/jchfs01'
alias cli_mt='run `git rev-parse --show-toplevel`/ext/linux/x86-64/release/bin/cli_mt 10.203.0.43:2181'
alias jtrader="/usr/java/jdk1.7.0_03/bin/java -cp JTrader.jar JTrader &"
alias ttr='`git rev-parse --show-toplevel`/run python `git rev-parse --show-toplevel`/t_trader/tt/ttrader/t_trader.py --stdout'
alias grp="git rev-parse --short"
alias myec2='aws ec2 describe-instances --region us-east-1 --filters "Name=tag-value,Values=tweiss"'
# alias chrome="/opt/google/chrome/google-chrome --enable-plugins &"
# alias chgenv='/opt/virtualenv/devws/bin/python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/change_environment.py '

alias algoconfig='cd /home/tweiss/dev-root/algo/build/x86-64/debug/etc/algo_local/'
alias newconfig='cd /etc/debesys'
alias algoconfigrel='cd /home/tweiss/dev-root/algo/build/x86-64/release/etc/algo_local/'
alias logsrel='cd /home/tweiss/dev-root/algo/build/x86-64/release/var/log/algo_local/'
alias logs='cd /home/tweiss/repo/dev-root/algo/x86-64/debug/var/log/algo_local/'
alias hsm='(cd /home/tweiss/dev-root/hsmproxy/bin; ./start.sh ../etc/hsmproxy.properties)&'

if [ -f $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/bashrc/devws.bash ]; then
    source $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/bashrc/devws.bash
fi
alias nutanix="devws_nutanix_server -vo"
alias deploy="devws_request_deploy -d 0"
alias tempvm='devws_tempvm --columns "VM Name" "Created Date" "Expiration Date" "Chef Env" "Runlist" "State"'

if [[ -f ~/bin/tig ]]; then
    alias tig=~/bin/tig
fi

set_display()
{
    # When tmux gets disconnected the DISPLAY environment variable often needs to be changed.
    pgrep Xorg > /dev/null
    if [ $? == 0 ]; then
        echo "X server is running (Xorg)."
    else
        echo "X server is not running (Xorg)."
    fi

    # if xlsclients returns 0 we're good otherwise we need to set $DISPLAY.
    xlsclients >> /dev/null
    if [ $? != 0 ]; then
        echo "DISPLAY is not set correctly ($DISPLAY)."
    fi
    echo DISPLAY was $DISPLAY.
    export DISPLAY="localhost:$1.0"
    echo DISPLAY is $DISPLAY.
}

function ja_test()
{
    jsonattr -s ${1} -j '{"jenkins_agent": {"verify_prod_pypi_access": false}}'
}

function csvs()
{
    find /tmp/compliance_* -name "*$1*csv" | xargs -n 1 -i ls -sh \{\}
}

function ekeu()
{
    knife environment from file deploy/chef/environments/$1 --config ~/.chef/knife.external.rb
}

function mtail()
{
    chef_env=$1
    cookbook=$2
    log_file=$3

    mtail_args=$(knife search node "chef_environment:${chef_env} AND recipe:${cookbook}" -a ipaddress 2>/dev/null | grep ipaddress | cut -d":" -f 2 | xargs -I '{}' -n 1 -i sh -c 'echo -l \"ssh $1 tail -f /var/log/debesys/${log_file}\"' - {} | tr "\n" " ")
    # x=$(knife search node "chef_environment:${chef_env} AND recipe:${cookbook}")
    eval multitail $mtail_args
}

function ecnow()
{
    knife ssh "chef_environment:ext-prod-live AND recipe:exchange_compliance*" "find /var/log/debesys -name compliance*log -mtime -0.05" -a ipaddress --config ~/.chef/knife.external.rb
}

function eknifessh ()
{
    eknife ssh "$1" "$2" --ssh-user $INTAD_USER --identity-file $INTAD_SSH_KEY -a ipaddress
}

function kitchenauth()
{
    rm -v ~/.aws-keys*
    $(/opt/virtualenv/devws3/bin/python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/aws_authenticator.py --account deb --role dev --env)
}

function vpn()
{
    if [ -z "$1" ]; then
        echo vpn on|off|status
        return
    fi

    if [[ $1 == "on" ]]; then
        /usr/bin/globalprotect connect
    fi

    if [[ $1 == "off" ]]; then
        /usr/bin/globalprotect disconnect
    fi

    if [[ $1 == "status" ]]; then
        /usr/bin/globalprotect show --status
    fi
}


function setchefconfig()
{
    if [ -z "$1" ]; then
        echo Invalid usage of setchefconfig, you must pass a hostname.
        return
    fi

    # Default to the internal organization.
    chef_config=~/.chef/knife.rb

    # Note to self: double brakets [[ ]] cause == to do wildcard matching and the behavior or == is
    # different with single brackets [ ].
    if [[ $1 == ar* || $1 == ch* || $1 == ny* || $1 = fr* ]]; then
        chef_config=~/.chef/knife.external.rb
    elif [[ $1 == sy* || $1 == sg* || $1 == ln* || $1 == hk* ]]; then
        chef_config=~/.chef/knife.external.rb
    elif [[ $1 == ty* || $1 == sp* || $1 == bk* || $1 == ba* ]]; then
        chef_config=~/.chef/knife.external.rb
    elif [[ $1 == se* || $1 == tw* ]]; then
        chef_config=~/.chef/knife.external.rb
    elif [[ $1 == *"ip-10-210"* ]]; then
        chef_config=~/.chef/knife.external.rb
    elif [[ $1 == *"ip-10-213"* ]]; then
        chef_config=~/.chef/knife.external.rb
    elif [[ $1 == *"ip-10-215"* ]]; then
        chef_config=~/.chef/knife.external.rb
    fi
}

function addtag2hosts__()
{
    local usage='Usage: addtag2hosts "new tag value" host1 host2 ... hostN'
    if [ -z "$1" ]; then
        echo $usage
        return
    fi

    if [ -z "$2" ]; then
        echo $usage
        return
    fi

    setchefconfig $2

    local query=""
    local first=0
    for var in "$@"
    do
        if [ $first == 0 ]; then
            first=1
            continue
        fi
        query+="name:$var OR "
    done
    query=$(echo -n $query | head -c -3)

    echo knife exec ~/dev-root/scripts/deploy/chef/scripts/snacks/add_tag.rb "$query" add "$1" --config $chef_config
    knife exec ~/dev-root/scripts/deploy/chef/scripts//snacks/add_tag.rb "$query" add "$1" --config $chef_config
}
alias addtag2hosts=addtag2hosts__

function addksme2hosts__()
{
    local usage='Usage: addksme2hosts host1 host2 ... hostN'
    if [ -z "$1" ]; then
        echo $usage
        return
    fi

    setchefconfig $1

    local query=""
    local first=0
    for var in "$@"
    do
        query+="name:$var OR "
    done
    query=$(echo -n $query | head -c -3)

    echo knife exec ~/dev-root/scripts/deploy/chef/scripts/snacks/add_tag.rb "$query" add kickstartme nousertag --config $chef_config
    knife exec ~/dev-root/scripts/deploy/chef/scripts//snacks/add_tag.rb "$query" add kickstartme nousertag --config $chef_config
}
alias addksme2hosts=addksme2hosts__

function addtag2query()
{
    local usage='Usage: addtag2query "new tag value" "data_center_name:aurora" [int|ext]'
    if [ -z "$1" ]; then
        echo $usage
        return
    fi

    if [ -z "$2" ]; then
        echo $usage
        return
    fi

    local chef_config=~/.chef/knife.rb
    if [ 'ext' == "$3" ]; then
        chef_config=~/.chef/knife.external.rb
    fi

    echo knife exec ~/dev-root/scripts/deploy/chef/scripts/snacks/add_tag.rb "$2" add "$1" --config $chef_config
    knife exec ~/dev-root/scripts/deploy/chef/scripts/snacks/add_tag.rb "$2" add "$1" --config $chef_config
}

function addrun2hosts__()
{
    local usage='Usage: addrun2hosts run_list_item host1 host2 ... hostN'
    if [ -z "$1" ]; then
        echo $usage
        return
    fi

    if [ -z "$2" ]; then
        echo $usage
        return
    fi

    setchefconfig $2

    local query=""
    local first=0
    for var in "$@"
    do
        if [ $first == 0 ]; then
            first=1
            continue
        fi
        query+="name:$var OR "
    done
    query=$(echo -n $query | head -c -3)

    echo knife exec ~/dev-root/scripts/deploy/chef/scripts/snacks/add_runlist.rb "$query" add "$1" --config $chef_config
    knife exec ~/dev-root/scripts/deploy/chef/scripts//snacks/add_runlist.rb "$query" add "$1" --config $chef_config
}
alias addrun2hosts=addrun2hosts__

function runlist()
{
    local usage='Usage: runlist host'
    if [ -z "$1" ]; then
        echo $usage
        return
    fi

    setchefconfig $1

    knife node show "$1" --config $chef_config -a run_list | grep run_list | sed 's/recipe\[//g' | sed 's/\]//g' | sed 's/run_list: base,//g' | tr "," " " | tr -s " " | sed 's/^[[:blank:]]*//g'
}

function addruns2host()
{
    local usage="Usage: addruns2host host run1 run2 ... runN\necho node1 node2 | tr \" \" \"\\\n\" | xargs -i -n 1 bash -cil \'addruns2host {} run1 run2 ... runN\'\nknife search node \"n:gla1vm85 OR n:gla2vm202\" -i 2> /dev/null | tr \" \" \"\\\n\" | xargs -i -n 1 bash -cil \'addruns2host {} run1 run2 ... runN\'\n"
    if [ -z "$1" ]; then
        printf "$usage"
        return
    fi

    if [ -z "$2" ]; then
        printf "$usage"
        return
    fi

    setchefconfig $1

    local first=0
    for var in "$@"
    do
        if [ $first == 0 ]; then
            first=1
            continue
        fi

        echo knife exec ~/dev-root/scripts/deploy/chef/scripts/snacks/add_runlist.rb "n:$1" add "$var" --config $chef_config
        knife exec ~/dev-root/scripts/deploy/chef/scripts/snacks/add_runlist.rb "n:$1" add "$var" --config $chef_config
    done
}

function emptyrunlist()
{
    local usage="Usage: emptyrunlist host1 host2 hostN\n"
    if [ -z "$1" ]; then
        printf "$usage"
        return
    fi

    setchefconfig $1

    for var in "$@"
    do
        echo knife exec ~/dev-root/scripts/deploy/chef/scripts/snacks/add_runlist.rb "n:$var" empty --config $chef_config
        knife exec ~/dev-root/scripts/deploy/chef/scripts/snacks/add_runlist.rb "n:$var" empty --config $chef_config
    done
}

function addattr2hosts()
{
    local usage="Usage: addattr2hosts attribute value host1 host2 ... hostN\n\nExample:\naddattr2hosts zookeeper_ensemble_name service_discovery gla3vm115 gla3vm128 gla3vm135\naddattr2hosts stealthwatch.logmaxretain 100 gla1vm187\naddattr2hosts algojob.overrides.use_price_unifier _1_ ar0srv100"
    if [ -z "$1" ]; then
        echo -e $usage
        return
    fi

    if [ -z "$2" ]; then
        echo -e $usage
        return
    fi

    if [ -z "$3" ]; then
        echo -e $usage
        return
    fi

    setchefconfig $3

    local query=""
    local first=0
    local second=0
    for var in "$@"
    do
        if [ $first == 0 ]; then
            first=1
            continue
        fi

        if [ $second == 0 ]; then
            second=1
            continue
        fi

        query+="name:$var OR "
    done
    query=$(echo -n $query | head -c -3)

    echo knife exec ~/dev-root/scripts/deploy/chef/scripts/snacks/add_attribute.rb "$query" add "$1" "$2" --config $chef_config
    knife exec ~/dev-root/scripts/deploy/chef/scripts//snacks/add_attribute.rb "$query" add "$1" "$2" --config $chef_config
}

function addattr2query()
{
    local usage="Usage: addattr2query attribute value quoted_query\n\nExample:\naddattr2query zookeeper_ensemble_name service_discovery \"chef_environment:ext-alt-live\"\naddattr2query stealthwatch.logmaxretain 100 \"name:sy*\"\naddattr2query algojob.overrides.use_price_unifier _1_ \"deployed_cookbooks:algoserver_exec\""
    if [ -z "$1" ]; then
        echo -e $usage
        return
    fi

    if [ -z "$2" ]; then
        echo -e $usage
        return
    fi

    if [ -z "$3" ]; then
        echo -e $usage
        return
    fi

    setchefconfig $3

    # local query=""
    # local first=0
    # local second=0
    # for var in "$@"
    # do
    #     if [ $first == 0 ]; then
    #         first=1
    #         continue
    #     fi

    #     if [ $second == 0 ]; then
    #         second=1
    #         continue
    #     fi

    #     query+="name:$var OR "
    # done
    # query=$(echo -n $query | head -c -3)

    echo knife exec ~/dev-root/scripts/deploy/chef/scripts/snacks/add_attribute.rb "$3" add "$1" "$2" --config $chef_config
    knife exec ~/dev-root/scripts/deploy/chef/scripts//snacks/add_attribute.rb "$3" add "$1" "$2" --config $chef_config
}


function rmattr__()
{
    local usage="Usage: rmattrhosts attribute host1 host2 ... hostN\n\nExample:\nrmattrhosts zookeeper_ensemble_name gla3vm115 gla3vm128 gla3vm135"
    if [ -z "$1" ]; then
        echo -e $usage
        return
    fi

    if [ -z "$2" ]; then
        echo -e $usage
        return
    fi

    setchefconfig $2

    local query=""
    local first=0
    for var in "$@"
    do
        if [ $first == 0 ]; then
            first=1
            continue
        fi

        query+="name:$var OR "
    done
    query=$(echo -n $query | head -c -3)

    echo knife exec ~/dev-root/scripts/deploy/chef/scripts/snacks/add_attribute.rb "$query" remove "$1" --config $chef_config
    knife exec ~/dev-root/scripts/deploy/chef/scripts//snacks/add_attribute.rb "$query" remove "$1" --config $chef_config
}
alias rmattrhosts=rmattr__


function eaddtag2query()
{
    addtag2query "$1" "$2" ext
}

function eaddtag()
{
    eknife exec $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/snacks/add_tag.rb
}

function kne()
{
    setchefconfig "$1"
    echo knife node edit "$1" --config $chef_config
    knife node edit "$1" --config $chef_config
}

function kns()
{
    setchefconfig "$1"
    if [ '-l' == "$2" ]; then
        echo "knife node show "$1" --config $chef_config --format json -l > /tmp/$1.json"
        knife node show "$1" --config $chef_config --format json -l > /tmp/$1.json
        $myemacs -nw /tmp/$1.json
    else
        echo knife search node "n:$1" --config $chef_config -a chef_environment -a run_list -a tags -a ipaddress -a platform_version
        knife search node "n:$1" --config $chef_config  -a chef_environment -a run_list -a tags -a ipaddress -a platform_version | sed 's/recipe\[//g' | sed 's/\]//g'
    fi
}

function extknife() { knife "$@" -c ~/.chef/knife.external.rb; }
alias eknife='extknife'

function spares__()
{
    usage='spares [host_prefix] (e.g., ar0srv)'
    if [ -z "$1" ]; then
        echo $usage
        return
    fi
    echo knife search node "chef_environment:ext-prod-sparepool AND name:$1*" -a platform_version -a tags --config ~/.chef/knife.external.rb
    knife search node "chef_environment:ext-prod-sparepool AND name:$1*" -a platform_version -a tags --config ~/.chef/knife.external.rb
}
alias spares=spares__

function find_spare()
{
    usage='find_spare [Chef Environment] [Data Center Prefix] [cookbook]'
    if [ -z "$1" ]; then
        echo $usage
        return
    fi

    if [ -z "$2" ]; then
        echo $usage
        return
    fi

    if [ -z "$3" ]; then
        echo $usage
        return
    fi

    echo knife exec ~/dev-root/scripts/deploy/chef/scripts/snacks/find_spare.rb "$1" "$2" "$3" --config ~/.chef/knife.external.rb
    knife exec ~/dev-root/scripts/deploy/chef/scripts/snacks/find_spare.rb "$1" "$2" "$3" --config ~/.chef/knife.external.rb
}
alias fspare=find_spare


function nodesize()
{
    local usage="Usage: nodesize [i|e] QUERY"
    if [ "$1" == 'i' ]; then
        chef_config=~/.chef/knife.rb
    elif [ "$1" == 'e' ]; then
        chef_config=~/.chef/knife.external.rb
    else
        echo -e $usage
        return
    fi

    if [ -z "$2" ]; then
        echo -e $usage
        return
    fi

    echo knife search node "$2" --config $chef_config -a cpu.total -a memory.total -a chef_environment -a run_list
    knife search node "$2" --config $chef_config -a cpu.total -a memory.total -a chef_environment -a run_list
}

function aws_keys()
{
    usage="aws_keys key_file"
    if [ -z "$1" ]; then
        echo $usage
        return
    fi

    if [ -f "$1" ]; then
        echo Loading AWS keys from "$1".
        source $1
    else
        echo Error: Didn\'t find "$1", couldn\'t load AWS keys.
    fi
}

function external()
{
    usage="external on|off"
    if [ -z "$1" ]; then
        echo $usage
        return
    fi

    if [ "on" == "$1" ]; then
        export PRE_EXTERNAL_PS1=$PS1
        export PRE_EXTERNAL_TERMINAL_TITLE=$CURRENT_TERMINAL_TITLE
        export PS1="\[\033[0;31m\]EXTERNAL DEBESYS\[\033[0;0m\] \h\[\033[1;30m\]\$(__git_ps1) \[\033[0;0m\]\w \n>"
        rename_terminal_title "EXTERNAL DEBESYS"
        alias ttknife='/opt/virtualenv/devws/bin/python `git rev-parse --show-toplevel`/ttknife -C ~/.chef/knife.external.rb'
        alias ttknife
        echo
        echo '######## ##     ## ######## ######## ########  ##    ##    ###    ##'
        echo '##        ##   ##     ##    ##       ##     ## ###   ##   ## ##   ##'
        echo '##         ## ##      ##    ##       ##     ## ####  ##  ##   ##  ##'
        echo '######      ###       ##    ######   ########  ## ## ## ##     ## ##'
        echo '##         ## ##      ##    ##       ##   ##   ##  #### ######### ##'
        echo '##        ##   ##     ##    ##       ##    ##  ##   ### ##     ## ##'
        echo '######## ##     ##    ##    ######## ##     ## ##    ## ##     ## ########'
        echo
        # http://patorjk.com/software/taag/#p=display&h=1&v=1&f=Banner3&t=EXTERNAL
        aws_keys ~/amazon_keys_ttnet.sh
        export EXTERNAL_DEBESYS="enabled"
    elif [ "off" == "$1" ]; then
        if [ ! -z "$PRE_EXTERNAL_PS1" ]; then
            export PS1=$PRE_EXTERNAL_PS1
        fi

        if [ $TMUX_PANE ]; then
            rename_terminal_title "bash"
        else
            if [ ! -z "PRE_EXTERNAL_TERMINAL_TITLE" ]; then
                rename_terminal_title "$PRE_EXTERNAL_TERMINAL_TITLE"
            fi
        fi

        alias ttknife='/opt/virtualenv/devws/bin/python `git rev-parse --show-toplevel`/ttknife'
        alias ttknife
        aws_keys ~/amazon_keys.sh
        export EXTERNAL_DEBESYS="disabled"
    else
        echo $usage
    fi
}

function tns()
{
    local hm=~
    local config="-C $hm/.chef/knife.rb"
    if [ "$EXTERNAL_DEBESYS" == "enabled" ]; then
        config=" -C $hm/.chef/knife.external.rb"
        echo external debesys
    fi
    echo "ttknife $config node show $1"
    ttknife $config node show "$1"
}

function rmchefnode()
{
    local hm=~
    local config="-C $hm/.chef/knife.rb"
    if [ "$EXTERNAL_DEBESYS" == "enabled" ]; then
        config=" -C $hm/.chef/knife.external.rb"
    fi

    if [ -z "$1" ]; then
        echo Usage: You must pass the node name.
        return
    fi

    echo "ttknife $config node delete --yes $1"
    /opt/virtualenv/devws/bin/python `git rev-parse --show-toplevel`/ttknife $config node delete --yes "$1"

    echo "ttknife $config client delete --yes $1"
    /opt/virtualenv/devws/bin/python `git rev-parse --show-toplevel`/ttknife $config client delete --yes "$1"
}

function search_chef_environment()
{
    local hm=~
    local config="--config $hm/.chef/knife.rb"
    if [ "$EXTERNAL_DEBESYS" == "enabled" ]; then
        config=" --config $hm/.chef/knife.external.rb"
    fi

    if [ -z "$1" ]; then
        echo Usage: You must pass the Chef environment and optionally a recipe.
        echo "Examples: sce int-dev-cert (all nodes in int-dev-cert)"
        echo "          sce int-dev-cert cme (all nodes in int-dev-cert with recipe cme)"
        return
    fi
    local search="functional_environment:$1"

    if [ ! -z "$2" ]; then
        search=$search" AND recipe:$2*"
    fi

    echo knife --config $config search node $search
    knife search node "$search" $config -a name -a chef_environment -a ipaddress -a run_list -a tags
}
alias sce=search_chef_environment

function ssh_to_chef_node()
{
    local usage="Usage: ssh_to_chef_node CHEF_ENVIRONMENT RECIPE"
    if [ -z "$1" ]; then
        echo $usage
        return
    fi

    if [ -z "$2" ]; then
        echo $usage
        return
    fi

    local knife_config=~/.chef/knife.rb
    if [[ $1 == ext-* ]]; then
        knife_config=~/.chef/knife.external.rb
    fi

    found=$(knife search node "chef_environment:$1 AND recipe:$2*" --config $knife_config -a ipaddress -a run_list --no-color)
    echo "$found"
    local ips=$(/opt/virtualenv/devws/bin/python $(git rev-parse --show-toplevel)/ttknife --config $knife_config search node "chef_environment:$1 AND recipe:$2*" -a ipaddress --no-color | grep ipaddress | tr -s " " | cut -d " " -f 3)

    echo ""
    echo Found: $ips

    PS3="Which $2: "
    select selection in $ips
    do
        # Commented out for now, but good way to verify ip address format.
        # if [ "$(sipcalc $ip | grep ERR)" != "" ]; then
        #     echo Sorry, $ip is not a valid IP address, aborting.
        #     return
        # fi

        echo ssh root@$selection
        ssh root@$selection
        break
    done
}
alias visit=ssh_to_chef_node

# function deploy__()
# {
#     local found_dash_a=false
#     # Example of bash substring match.
#     if [[ "$@" == *"-a"* ]]; then
#         found_dash_a=true
#     fi

#     local found_dash_h=false
#     if [[ "$@" == *"-h"* ]]; then
#         found_dash_h=true
#     fi

#     if [ $found_dash_h == false -a $found_dash_a == false ]; then
#         local title_start="deploying..."
#         local window=`tmux list-windows | grep "\(active\)" | cut -d" " -f 1 | sed s'/://g'`
#         rename_terminal_title "$title_start"
#     fi

#     echo $DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/request_deploy.py "$@"
#     $DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/request_deploy.py "$@"

#     if [ $found_dash_h == false -a $found_dash_a == false ]; then
#         local title_done="deploying...done"
#         rename_terminal_title "$title_done" "$window"
#     fi
# }
# alias deploy=deploy__

function set_many_rcv()
{
    if [ -z "$1" ]; then
        echo Usage: You must pass the query \(quoted\) and some number of cookbooks.
        echo "Example: esetrcvs \"chef_environment:ext-uat-cert\" cme ice"
        return
    fi

    local query="$1"
    local first=false
    for COOKBOOK in "$@"
    do
        if [ $first == false ]; then
            # The first argument is the query, skip it.
            first=true
            continue
        fi

        echo knife exec $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/snacks/set_rc_version.rb \"$query AND deployed_cookbooks:$COOKBOOK\" $COOKBOOK --config ~/.chef/knife.external.rb
        knife exec $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/snacks/set_rc_version.rb "$query AND deployed_cookbooks:$COOKBOOK" $COOKBOOK --config ~/.chef/knife.external.rb
    done

}
alias esetrcvs=set_many_rcv

function find_environments()
{
    if [ -z "$1" ]; then
        echo Usage: You must pass a cookbook
        return
    fi

    knife search node "deployed_cookbooks:$1" -a chef_environment --config ~/.chef/knife.rb 2> /dev/null | grep environment | sed 's/chef_environment: //g' | sed 's/ //g' | sort | uniq
    knife search node "deployed_cookbooks:$1" -a chef_environment --config ~/.chef/knife.external.rb 2> /dev/null | grep chef_environment | sed 's/chef_environment: //g' | sed 's/ //g' | sort | uniq
}
alias fenv=find_environments

function distmake() {
   reporoot=$(git rev-parse --show-toplevel)
   chef_distcc_get_hosts_script="$reporoot/deploy/chef/cookbooks/distcc/files/default/get_distcc_hosts.rb"
   test -f $chef_distcc_get_hosts_script && knife exec $chef_distcc_get_hosts_script
   make c_compiler="distcc /opt/build/gcc-4.9.1/bin/gcc" cpp_compiler="distcc /opt/build/gcc-4.9.1/bin/g++" "$@"
}
# distmake -j16 <target>

# To view the definition of a function, do 'type <function>'.
function cf() { $myemacsclient -n `find . -name $1`; }
function f() { find . -name $1 -print; }
function rmbr()
{
    for do_not_delete in master origin/master uat/current origin/uat/current release/current origin/release/current develop origin/develop
    do
        if [ $do_not_delete == "$1" ]; then
            echo "Oops! I think you are accidentally trying to delete one of the important branches, aborting."
            return
        fi
    done

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


function chef-node-report()
{
    local report=~/chef-node-report.txt
    if [ -f $report ]; then
	rm -v $report;
    fi
    ttknife node list | xargs -n 1 knife node show | sed --unbuffered 's/Node Name:/\nNode Name:/g' | tee $report;
    ttknife node list | xargs -n 1 knife node show | grep "IP:" --line-buffered | tee $report;
    # ttknife node list | xargs -n 1 knife node show | grep "IP:" --line-buffered | tr -s " " | cut -d" " -f 2 | xargs -n 1 -i ping -c 5 -q \{\} | grep -iE "statistics|packet" >> $report;
}

function git-sync_()
{
    usage="git-sync branch"
    if [ -z "$1" ]; then
        echo $usage
        return
    fi

    git tt co $1

    # local branch="$1"
    # if [ "d" == "$1" ]; then
    #     branch="develop"
    # elif [ "r" == "$1" ]; then
    #     branch="release/current"
    # elif [ "u" == "$1" ]; then
    #     branch="uat/current"
    # elif [ "m" == "$1" ]; then
    #     branch="master"
    # fi

    echo "pushd `git rev-parse --show-toplevel`";
    pushd `git rev-parse --show-toplevel`;
    if [ $? != 0 ]; then
        echo "Aborting."
        echo "popd"; popd;
        return
    fi
    # echo "git remote prune origin";
    # git remote prune origin;
    # if [ $? != 0 ]; then
    #     echo "Aborting."
    #     echo "popd"; popd;
    #     return
    # fi
    # echo "git checkout $branch";
    # git checkout "$branch";
    # if [ $? != 0 ]; then
    #     echo "Aborting."
    #     echo "popd"; popd;
    #     return
    # fi
    # echo "git pull";
    # git pull;
    # if [ $? != 0 ]; then
    #     echo "Aborting."
    #     echo "popd"; popd;
    #     return
    # fi
    # echo "git submodule update --init --recursive";
    # git submodule update --init --recursive;
    # if [ $? != 0 ]; then
    #     echo "Aborting."
    #     echo "popd"; popd;
    #     return
    # fi
    echo "popd"; popd;
}

function cpcfg_()
{
    cp -v `git rev-parse --show-toplevel`/config/lbm_config_lo.xml /etc/debesys/lbm.conf;
    cp -v `git rev-parse --show-toplevel`/config/lbm_config_lo.xml /etc/debesys/lbm.local.conf;
    cp -v ~/mnt/d30/etc/debesys/lbm.conf /etc/debesys/lbm.int-dev-cert.conf
}
alias cpcfg=cpcfg_

function int-dev-cert-lbm_()
{
    rm -v /etc/debesys/env_is
    rm -v /etc/debesys/lbm.conf;
    cp -v /etc/debesys/lbm.int-dev-cert.conf /etc/debesys/lbm.conf;
    echo "int-dev-cert" > /etc/debesys/env_is
    cat /etc/debesys/env_is
}
alias lbm-int-dev-cert=int-dev-cert-lbm_

function locallbm_()
{
    rm -v /etc/debesys/env_is
    sudo ifconfig lo multicast # ensure multicast is enabled on loopback
    rm -v /etc/debesys/lbm.conf;
    cp -v /etc/debesys/lbm.local.conf /etc/debesys/lbm.conf;
    echo "local" > /etc/debesys/env_is
    cat /etc/debesys/env_is
}
alias lbm-local=locallbm_

function m_()
{
    # By default, set the number of cpus to be all of them except
    # one.
    local cpus=$(expr `nproc` - 1)
    if [ "x$MAKE_CPUS_OVERRIDE" != "x" ]; then
        cpus=$MAKE_CPUS_OVERRIDE
        echo "(MAKE_CPUS_OVERRIDE causes -j $cpus)"
    fi

    # The $@ variable contains all the arguments.  The parenthesis run in a subshell
    # which keeps the effect of set -x (echoing commands) from being permanent.
    ( set -x; time make -Rr -j $cpus -C `git rev-parse --show-toplevel` "$@" )
    if [ $? == 0 ]; then
        echo COMPILE SUCCESSFUL!
    else
        echo COMPILE FAILED!
        return 1
    fi
}
alias m=m_

function mice_()
{
    m_ 'all_price'
}
alias mice=mice_

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
        $myemacs "$@" &
    else
        echo emacs is running, sending $@
        echo $isemacs;
        $myemacsclient -n "$@"
    fi
}
alias em=em_

function mkchefec2()
{
    if [ -z "$1" ]; then
        echo 'Usage: you must pass the node name, mkchefec2 node_name [size]'
        return
    fi

    pushd `git rev-parse --show-toplevel`

    if [ -z "$2" ]; then
        ebs_size="--ebs-size 20"
    else
        ebs_size="--ebs-size $2"
    fi

    echo /opt/virtualenv/devws/bin/python deploy/chef/scripts/ec2_server.py --size m1.medium --ami ami-eb6b0182 --dept debesys --cost-center "Engineering-490" --project debesys --manager "Tom Weiss" --user root --environment int-dev-sparepool --recipe base $ebs_size -a $1
    /opt/virtualenv/devws/bin/python deploy/chef/scripts/ec2_server.py --size m1.medium --ami ami-eb6b0182 --dept debesys --cost-center "Engineering-490" --project debesys --manager "Tom Weiss" --user root --environment int-dev-sparepool --recipe base $ebs_size -a $1

    local ip=`knife node show $1 | grep IP | tr -s ' ' | cut -d" " -f 2`
    if [ -z ip ]; then
        echo "Not able to find IP address of AWS instance."
        return
    fi

    # In order to run the sshfs command with the user root, we need to replace root's
    # authorized_keys with ec2-user's authorized_keys.  The root use does not otherwise
    # allow for mounting the file system with write permission.
    # if [ "rhel" == "$2" ]; then
    #     echo ssh -t ec2-user@$ip -i ~/.ssh/aws.pem "sudo cp /root/.ssh/authorized_keys /root/.ssh/authorized_keys_orig"
    #     ssh -t ec2-user@$ip -i ~/.ssh/aws.pem "sudo cp /root/.ssh/authorized_keys /root/.ssh/authorized_keys_orig"
    #     echo ssh -t ec2-user@$ip -i ~/.ssh/aws.pem "sudo cp /home/ec2-user/.ssh/authorized_keys /root/.ssh/authorized_keys"
    #     ssh -t ec2-user@$ip -i ~/.ssh/aws.pem "sudo cp /home/ec2-user/.ssh/authorized_keys /root/.ssh/authorized_keys"
    # fi

    # mkdir -pv ~/mnt/$1
    # echo sshfs root@$ip:/ ~/mnt/$1 -o IdentityFile=~/.ssh/aws.pem
    # sshfs root@$ip:/ ~/mnt/$1 -o IdentityFile=~/.ssh/aws.pem

    popd
}

function rmchefec2()
{
    if [ -z "$1" ]; then
        echo Usage: you must pass the node name.
        return
    fi

    pushd `git rev-parse --show-toplevel`

    echo /opt/virtualenv/devws/bin/python deploy/chef/scripts/ec2_server.py -d $1
    /opt/virtualenv/devws/bin/python deploy/chef/scripts/ec2_server.py -d $1

    # echo sudo umount ~/mnt/$1
    # sudo umount ~/mnt/$1

    # echo rmdir ~/mnt/$1
    # rmdir ~/mnt/$1

    popd
}

function upld()
{
    if [ -z "$1" ]; then
        echo Usage: you must pass the tag.
        return
    fi

    pushd `git rev-parse --show-toplevel`
    rm orders/cme/source/*.fix.* -v
    m_ config=release clean
    rm -rfv build
    /opt/virtualenv/devws/bin/python ./deploy/chef/scripts/upload_debesys.py --tag $1
    popd
}

function knf()
{
    git rev-parse --show-toplevel >> /dev/null
    if [ $? != 0 ]; then
        echo "You need to be in a repository to run knife."
        return
    fi

    # If any of the arguments contains the substring 'ssh', then directly call knife.
    if [[ $@ == *ssh* ]]; then
        /usr/bin/knife "$@"
        return
    fi

    pushd `git rev-parse --show-toplevel`/deploy/chef >> /dev/null
    if [ $? != 0 ]; then
        return
    fi
    /usr/bin/knife "$@"
    popd >> /dev/null
}

function pes()
{
    if [ -z "$1" ]; then
        echo "Usage: pes [on|off] node"
        return
    fi
    if [ -z "$2" ]; then
        echo "Usage: pes [on|off] node"
        return
    fi

    if [ "on" == "$1" ]; then
        knife exec `git rev-parse --show-toplevel`/deploy/chef/scripts/snacks/add_attribute.rb "name:$2" add haproxy.skip_haproxy _true_ 2> /dev/null
        knife exec `git rev-parse --show-toplevel`/deploy/chef/scripts/snacks/add_attribute.rb "name:$2" add haproxy.weight 0 2> /dev/null
        knife exec `git rev-parse --show-toplevel`/deploy/chef/scripts/snacks/add_attribute.rb "name:$2" add edgeserver.disable_smoke_test _true_ 2> /dev/null
    fi

    if [ "off" == "$1" ]; then
        knife exec `git rev-parse --show-toplevel`/deploy/chef/scripts/snacks/add_attribute.rb "name:$2" remove haproxy 2> /dev/null
        knife exec `git rev-parse --show-toplevel`/deploy/chef/scripts/snacks/add_attribute.rb "name:$2" remove edgeserver 2> /dev/null
    fi
}

function rename_terminal_title_no_prefix()
{
    if [ -z "$1" ]; then
        echo Usage: You must pass the new title.
        return
    fi
    echo -en "\033]0;$1\007"
}

function rename_terminal_title()
{
    if [ -z "$1" ]; then
        echo Usage: You must pass the new title.
        return
    fi

    if [ -z "$2" ]; then
        local tmux_window=""
    else
        local tmux_window="-t $2"
    fi

    local title="term | $1"
    local tmux_title_no_spaces=$(echo "$1" | sed -e 's/ /_/g')

    if [ $TMUX_PANE ]; then
        tmux rename-window $tmux_window $tmux_title_no_spaces
        tmux refresh-client
    else
        echo -en "\033]0;$title\007"
        export CURRENT_TERMINAL_TITLE="$1"
    fi
}
alias rw=rename_terminal_title

# No auto rename on a new tmux.
# if [ ! $TMUX_PANE ]; then
#     rename_terminal_title ":-)"
# fi

csview()
{
    local file="$1"
    cat "$file" | sed -e 's/,,/, ,/g' | column -s, -t | less -#5 -N -S
}

function gitclean()
{
    git tag | xargs -n 1 git tag -d
    echo git fetch
    git fetch
    echo git gc
    git gc
}

function gitcleanall()
{
    for repo in `/bin/ls ~/dev-root`;
    do
        pushd ~/dev-root/$repo
        echo "Cleaning repo $repo."
        gitclean
        popd
    done
}

function cbv()
{
    rm /tmp/cbv > /dev/null 2>&1
    git show origin/$2:deploy/chef/cookbooks/$1/metadata.rb > /tmp/cbv
    grep version /tmp/cbv
}

function gg()
{
    git rev-parse --verify tweiss_gg > /dev/null 2>&1
    if [ $? == 0 ]; then
        echo "Found tweiss_gg, deleting via 'git branch -D tweiss_gg'."
        git branch -D tweiss_gg
        echo "Deleting from remote via 'git push origin --delete tweiss_gg'."
        git push origin --delete tweiss_gg
    fi

    echo "git checkout -b tweiss_gg"
    git checkout -b tweiss_gg
}

function check_envs()
{
    mergetest
    for env_file in 'ext-prod-live' 'ext-prod-sim' 'ext-prod-delayed' 'ext-prod-md-pp' 'ext-prod-cassandra' 'ext-prod-coreinfra' 'ext-prod-live-eex' 'ext-prod-md-pp-delayed' 'ext-prod-md-pp-eex' 'ext-prod-mon' 'ext-prod-other-cassandra' 'ext-prod-sparepool' 'int-stage-cert-master' 'int-stage-md-sp-master' 'int-prod-sim' 'int-prod-md-pp';
    do
        echo "Checking $env_file."
        git diff master:deploy/chef/environments/$env_file.rb release/current:deploy/chef/environments/$env_file.rb
        git diff release/current:deploy/chef/environments/$env_file.rb develop:deploy/chef/environments/$env_file.rb
    done

    for env_file in 'int-stage-cert' 'int-stage-md-sp';
    do
        echo "Checking $env_file."
        git diff release/current:deploy/chef/environments/$env_file.rb develop:deploy/chef/environments/$env_file.rb
    done
}

function awsauth()
{
    $(/opt/virtualenv/devws3/bin/python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/aws_authenticator.py --account deb --role read --env)
}

function cpr()
{

    echo "https://github.com/tradingtechnologies/debesys/pull/new/`git rev-parse --abbrev-ref HEAD`"
}

function chg()
{
    printf "/opt/virtualenv/devws3/bin/python \$DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/commander.py --knife-config ~/.chef/knife.external.rb -e -c -s -r -C '' --test-run\\n\\n/opt/virtualenv/devws3/bin/python \$DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/update_environment.py --support-locks -r  -c  -e ext- \\n\\n/opt/virtualenv/devws3/bin/python \$DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/knife_ssh.py --knife-config ~/.chef/knife.external.rb --audit-runlist --concurrency 50 -a -e  -r  --test-run\\n\\n/opt/virtualenv/devws3/bin/python \$DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/modify_runlist.py -o ext -s -r run1 run2 runN --chgnum \\n\\n/opt/virtualenv/devws3/bin/python \$DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/add_json_attribute.py -o ext -e ext- -s  -r -j ''\\n\\n/opt/virtualenv/devws3/bin/python \$DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/view_changes.py -c cb@x.y.z -e ext-prod- -r\\n\\n" | xclip
}

function xbump()
{
    usage="xbump [cookbook] [r1 r2 rN]\nFor each release, checkout the branch and bump the cookbook.\n"
    if [[ -z "$1" ]]; then
        printf "$usage"
        return
    fi

    if [[ -z "$2" ]]; then
        printf "$usage"
        return
    fi

    local first=0
    for release in "$@"
    do
        if [[ $first == 0 ]]; then
            first=1
            continue
        fi

        git tt co $release
        devws_bump_cookbook -c $1 -n
    done
}

function eftxfer()
{
    usage="eftxfer COMPANY_ID\n"
    if [[ -z "$1" ]]; then
        printf "$usage"
        return
    fi

    homedir=~
    eval homedir=$homedir

    if [[ ! -d $homedir/compliance/${1} ]]; then
        echo "Directory $homedir/compliance/${1} not found!"
        return
    fi

    if [[ ! -f $homedir/.ssh/scp_private_key.pem ]]; then
        echo "Missing authentication key $homedir/.ssh/scp_private_key.pem!"
        return
    fi

    transfer_count=`/bin/ls -1 $homedir/compliance/${1} | wc -l`
    echo "Transferring $transfer_count files from $homedir/compliance/${1} to /TT_NextGen/tt_company_${1} in 10 seconds."
    for i in $(seq 1 10); do echo -n ".";  sleep 1; done
    echo ""

    echo "progress" > /tmp/transfer.${1}.sftp
    echo "cd /TT_NextGen/tt_company_${1}" >> /tmp/transfer.${1}.sftp
    echo "put $homedir/compliance/${1}/*" >> /tmp/transfer.${1}.sftp

    sftp -oIdentityFile=$homedir/.ssh/scp_private_key.pem Debesys@10.111.0.200 < /tmp/transfer.${1}.sftp

    echo ""
    echo "Transfer completed."
    echo ""
    echo "Your EFT compliance restore is complete (${transfer_count} files were restored)."
    echo "Restores happen in chunks of a month (you may get more files than requested)."
    echo "The files are available in your company's EFT folder (company id ${1}).  Please"
    echo "make sure to collect these files as they will automatically be purged after 30 days."
    echo "Also please ensure newly created files are being regularly collected."

}

function dedicated()
{
    commands="The following commands were used to generate the data:\n"

    printf "Report on Dedicated JPM hosts. @tom.mckee @melissa.waitz @russ.cotton @ryan.ohnemus\n\n"

    for cb in orderrtgnode backofficenode fixorderrouting fixdropcopy
    do
        commands="${commands}knife search node \"chef_environment:ext-prod-live AND recipe:${cb} AND haproxy:* AND recipe:*dedicated_for_jpm\" -a run_list -a base.groups -a cookbook_deployment_data.${cb}.version -a cookbook_deployment_data.${cb}.installed --config ~/.chef/knife.external.rb --no-color 2> /dev/null\n\n"
        knife search node "chef_environment:ext-prod-live AND recipe:${cb} AND haproxy:* AND recipe:*dedicated_for_jpm" -a run_list -a base.groups -a cookbook_deployment_data.${cb}.version -a cookbook_deployment_data.${cb}.installed --config ~/.chef/knife.external.rb --no-color 2> /dev/null
    done


    for cb in hkex tfex ose sgx asx_ntp bouncer
    do
        commands="${commands}knife search node \"chef_environment:ext-prod-live AND recipe:*dedicated_for_jpm AND recipe:${cb}*\" -a run_list -a base.groups -a cookbook_deployment_data.${cb}.version -a cookbook_deployment_data.${cb}.installed --config ~/.chef/knife.external.rb --no-color 2> /dev/null\n\n"
        knife search node "chef_environment:ext-prod-live AND recipe:*dedicated_for_jpm AND recipe:${cb}*" -a run_list -a base.groups -a cookbook_deployment_data.${cb}.version -a cookbook_deployment_data.${cb}.installed --config ~/.chef/knife.external.rb --no-color 2> /dev/null
    done


    printf "\n$commands \n"
}

function deployed_releases()
{
    if [ -z "$1" ]; then
        echo Invalid usage of deployed_releses, you must pass the initial release number.
        return
    fi

    for index in $(seq 30 1 ${1} | tac);
    do
        echo Checking for release ${index} cookbooks that are still deployed...
        echo "knife search node \"*\" -a deployed_cookbooks --config ~/.chef/knife.external.rb 2>/dev/null | grep \"\.${index}\.\" | wc -l"
        count=$(knife search node "*" -a deployed_cookbooks --config ~/.chef/knife.external.rb 2>/dev/null | grep "\.${index}\." | wc -l)
        knife search node "*" -a deployed_cookbooks --config ~/.chef/knife.external.rb 2>/dev/null | grep "\.${index}\." | tr -s " " | sort -u
        echo "Nodes found: $count"
        echo ""
    done

}

function fix_audit()
{
    for cb in orderrtgnode fixorderrouting backofficenode fixdropcopy secdefnode fixmarketdata miaminode binarymarketdata
    do
        printf "\n***** $cb *****\n"
        knife search node "chef_environment:ext-prod-live AND recipe:${cb}*" -a run_list -a base.groups -a instance_identifier.dedicated --config ~/.chef/knife.external.rb
    done
}

function envv()
{
    if [ -z "$1" ]; then
        echo 'Invalid usage of envv: envv COOKBOOK int|ext.'
        return
    fi

    if [ -z "$2" ]; then
        echo 'Invalid usage of envv: envv COOKBOOK int|ext.'
        return
    fi

    grep \"${1}\" deploy/chef/environments/${2}* | grep -vE "ext-uat-sim|ext-uat-sparepool|algo-backtest|tmorris|int-perf|int-dev-perf|jenkins|stage-sparepool|pp-etl" | sed 's:deploy/chef/environments/::g' | sed 's/.rb://g' | sed 's/",//g' | tr -s " " | cut -d" " -f 1,5 | awk '{print $2, $1}' | column -t | sort
}

function workstations()
{
    knife search node "chef_environment:int-dev-workstation" -a devws_base.intad_user -a devws_base.lbm -a devws_base.skip_lbm
}

function replication()
{
    if [[ $HOSTNAME == jchi* ]]; then
        echo "mount /home/tweiss/jch76vm40"
        mount /home/tweiss/jch76vm40
    else
        echo "sudo mount.cifs -o user=tweiss //CHIJCHFS01.int.tt.local/Share /mnt/CHIJCHFS01"
        sudo mount.cifs -o user=tweiss //CHIJCHFS01.int.tt.local/Share /mnt/CHIJCHFS01
    fi
}

function fixfix()
{
    # Convert FIX delimiter into something more readable.
    cat ${1} | tr '\01' "|"
}

function audit_spares()
{
    for DC in ar ch ny sp ln ba fr sg hk bk ty sy se tw; do
        /opt/virtualenv/devws3/bin/python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/spare.py -e ext-prod-live -d ${DC} -c cb_placeholder --tenancy genpool | less
    done

    knife search node "chef_environment:ext-prod-sparepool AND n:*vm*" -a tags -a run_list -a creation_info.date --config ~/.chef/knife.external.rb | less
}

#
# Virtual Box Notes
#
# System -> Motherboard -> 4GB RAM (depends on your needs)
# System -> Processor -> 4 CPUs (depends on your needs)
# Network -> Adapter 1 -> NAT
# Network -> Adapter 1 -> Port Forwarding (port 22 on host to port 22 on guest)
# Network -> Adapter 1 -> Cable Connected (check box)
# Storage -> Controller: IDE -> Click on disk to load the iso Centos isos are available here.  These
# instructions support the minimal iso, but are also appropriate to the live dvd iso.
# Power On
# Devices -> Shared Clipboard -> Bidirectional
# Start the networking: ifup eth0
#
# edit /etc/sysconfig/network-scripts/ifcfg-eth0
#
# DEVICE=eth0
# TYPE=Ethernet
# BOOTPROTO=dhcp
# ONBOOT=yes
# NM_CONTROLLED=no
# PEERDNS=no
#
# edit  /etc/inittab and change
# id:3:initdefault:
# to
# id:5:initdefault:


# Author.: Ole J
# Date...: 23.03.2008
# License: Whatever

# Wraps a completion function
# make-completion-wrapper <actual completion function> <name of new func.>
#                         <command name> <list supplied arguments>
# eg.
#   alias agi='apt-get install'
#   make-completion-wrapper _apt_get _apt_get_install apt-get install
# defines a function called _apt_get_install (that's $2) that will complete
# the 'agi' alias. (complete -F _apt_get_install agi)
#
function make-completion-wrapper () {
    local function_name="$2"
    local arg_count=$(($#-3))
    local comp_function_name="$1"
    shift 2
    local function="
function $function_name {
    ((COMP_CWORD+=$arg_count))
    COMP_WORDS=( "$@" \${COMP_WORDS[@]:1} )
    "$comp_function_name"
    return 0
}"
    eval "$function"
}

alias gits='git-sync_'
make-completion-wrapper _git _git_checkout_mine git checkout
complete -o bashdefault -o default -o nospace -F _git_checkout_mine gits
complete -o bashdefault -o default -o nospace -F _git_checkout_mine rmbr
make-completion-wrapper _git _git_mine git
alias g='git'
complete -o bashdefault -o default -o nospace -F _git_mine g

function merge()
{
    local branches=$(git branch --no-color | awk -F ' +' '{print $2}' | grep -v master | grep -v uat/current | grep -v release/current | grep -v develop)
    if [ "" == "$branches" ]; then
        echo 'There are no candidate branches to merge. The only branches found were the git flow branches.'
        echo Found: $(git branch | awk -F ' +' '{print $2}')
        return
    fi

    PS3="Which branch do you want to merge: "
    select selection in $branches
    do
        echo git merge --no-ff $selection
        git merge --no-ff $selection
        break
    done

    if [ $? != 0 ]; then
        echo "The merge failed."
    else
        echo "The merge was successful."
    fi

    # Instead of injecting the function merge into the history, inject the resulting git command
    # with the branch into the history.  The reason this is better is that when performing the git
    # flow merging, I can up-arrow to perform the merge on the other git flow branches.  Without
    # this, the command merge is in the history and it prompts me, but I know the one I want to
    # merge cause I just chose it.
    history -s "git merge --no-ff $selection"
}

function edit_on_branch()
{
    local files=$(git status --no-color | awk -F ' +' '{print $2}' | grep -v master | grep -v uat/current | grep -v release/current | grep -v develop)
    # if [ "" == "$branches" ]; then
    #     echo 'There are no candidate branches to merge. The only branches found were the git flow branches.'
    #     echo Found: $(git branch | awk -F ' +' '{print $2}')
    #     return
    # fi

    # PS3="Which branch do you want to merge: "
    # select selection in $branches
    # do
    #     echo git merge --no-ff $selection
    #     git merge --no-ff $selection
    #     break
    # done

    # if [ $? != 0 ]; then
    #     echo "The merge failed."
    # else
    #     echo "The merge was successful."
    # fi

}


# Uncomment to debug command to see when this file is sourced.
# if [ ! -f /var/log/profiles ]
# then
#     if [ -w /var/log/profiles ]; then
#         sudo touch /var/log/profiles
#         sudo chmod a+rw /var/log/profiles
#     fi
# fi

# if [ -w /var/log/profiles ]; then
#     echo .bashrc ran at $(date) >> /var/log/profiles
# fi

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

#echo DISPLAY is $DISPLAY.
#echo sudo netstat -tulpn | grep "127.0.0.0:60"
#sudo netstat -tulpn | grep "127.0.0.0:60"
#echo ps -ef | grep sshd | grep tweiss@
# ps -ef | grep sshd | grep tweiss@
#echo "Run killmyssh to kill all current sessions."
alias killmyssh='ps -ef | grep sshd | grep tweiss@ | tr -s " " | cut -d" " -f 2 | xargs kill'
# If I do 'pkill -u tweiss' that will kill all my sessions and get sshd back on display 10.

# wmctrol -lp will list windows and their process ids
# Find chrome's window name.
# wmctrl -lp | tr -s " " | cut -d " " -f 3

# Created by `pipx` on 2021-05-26 15:07:17
export PATH="$PATH:/home/tweiss/.local/bin"
