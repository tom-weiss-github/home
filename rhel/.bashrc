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
ulimit -c unlimited

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


# History across terminal sessions.
export HISTSIZE=10000
shopt -s histappend
# history -a => append current session's history to history file (happens at session exit)
# history -c => clear the current session's history
# history -r => reload the history from the history file into the current session
# Commenting this out because it seems that my terminal history has the wrong numbers so I can't !
# number anymore.  PROMPT_COMMAND="history -a;history -c;history -r;$PROMPT_COMMAND"
export HISTTIMEFORMAT='%F %T '

export EDITOR="emacs -nw"
# ALTERNATE_EDITOR causes emacs to be opened if emacsclient is invoked and no instance is running.
export ALTERNATE_EDITOR=emacs
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
export PATH=$PATH:~/Downloads/meld-1.6.1/bin
export PATH=$PATH:/opt/scala-2.9.3/bin/
export INTAD_USER=tweiss
export BCV_ENABLE_LDAP=1
export VCD_ORG=Dev_General
export JENKINS_USER=tom.weiss@tradingtechnologies.com
if [ -f ~/jenkins_token ]; then
    export JENKINS_TOKEN=$(head -n 1 ~/jenkins_token)
fi
export DEPLOYMENT_SCRIPTS_REPO_ROOT=~/dev-root/scripts
# export BUMP_COOKBOOK_VERSION_ALTERNATE_REPO=~/dev-root/cookbooks
export REQUEST_BUILD_SUPPRESS_TIPS=1
export BUMP_COOKBOOK_VERSION_AUTO_EXECUTE=1
export FEATURE_TEST_EMAIL=tom.weiss@tradingtechnologies.com
# To run ringer:
# cp ringer.conf/srl_config_ringer.xml from some machine in int-dev-sim
# cp deploy/chef/cookbooks/srlabs/files/default/smds.lic /etc/debesys/
export JAVA_HOME=/usr/java/jdk1.7.0_17
# run /usr/java/jdk1.7.0_17/bin/java -Dversion="0.0.0" -cp ./ringer/target/Ringer.jar Ringer --srl-config /etc/debesys/srl_config_ringer.xml -v -o

alias off='sudo shutdown -P now'
alias todo='emacs -nw ~/todo.txt'
alias rooms='cat ~/githome/rooms.txt'
alias sb='source ~/.bashrc'
alias edbrc='emacs -nw ~/githome/rhel/.bashrc'
alias ee='emacs -nw'
alias c='emacsclient -n'
alias ls='ls -aFCh --color=always'
alias h='history | tail -n 50'
alias hg='history | grep'
alias rwbr='~/githome/setxtitle.sh $(__git_ps1)'
alias vm16='ssh tweiss@10.202.0.16 -i ~/.ssh/id_rsa'
alias clk='python ~/githome/world_time.py'
alias gdb='gdb -n'
alias gt='gnome-terminal &'
alias swarm="$DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/swarm.py --verbose "
alias vcloud="$DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/vcloud_server.py"
alias nutanix="$DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/nutanix_server.py -o "
alias bcv="$DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/bump_cookbook_version.py"
alias ec2="$DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/ec2_instance.py -v --route53 "
alias mergetest="$DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/check_repo.py"
alias fta="$DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/feature_test_assistant.py"
alias smile='rename_terminal_title ":-)"'
alias prdp='echo "@bcordonn @elmedinam @jkess @joanne-wilson @srubik @TIMSTACY @jfrumkin @jerdmann" | xclip -selection clipboard'
alias proc='echo "@mdw55189 @corystricklin @jingheelu @lmancini54" | xclip -selection clipboard'
alias git-commit-hook="cp ~/githome/prepare-commit-msg .git/hooks/; chmod a+x .git/hooks/prepare-commit-msg"
alias tkw="tmux kill-window"
alias tkp="tmux kill-pane"
alias tsud="tmux split-window"
alias tnw="tmux new-window"
alias tks="tmux kill-server"
alias fakechef="cp -v ~/.chef/knife.training.rb.orig ~/.chef/knife.rb && cp -v ~/.chef/knife.training.rb.orig ~/.chef/knife.external.rb && export BUMP_COOKBOOK_VERSION_NO_NOTES=1 && echo BUMP_COOKBOOK_VERSION_NO_NOTES has been set."
alias realchef="cp -v ~/.chef/knife.rb.orig ~/.chef/knife.rb && cp -v ~/.chef/knife.external.rb.orig ~/.chef/knife.external.rb && unset BUMP_COOKBOOK_VERSION_NO_NOTES && echo BUMP_COOKBOOK_VERSION_NO_NOTES has been unset."


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
alias ff="find . -type d $ff_dir \( $ff_file \) -print0 | xargs -0 grep -iHn"

alias git-add-mod='git status | grep modified | cut -d " " -f 4 | xargs --max-args=1 git add -v'
alias allbranches="git for-each-ref --format='%(committerdate) %09 %(authorname) %09 %(refname)' | sort -k5n -k2M -k3n -k4n"
alias glog='git glog -13'
alias galias='git config --list | grep alias'
alias soc='kill `cat /var/run/cme.pid`'
alias oc?='cat /var/run/cme.pid; ps -ef | grep cme | grep -v grep'
alias emacs?='ps -ef | grep emacs | grep -v "grep emacs" | grep -v "emacs -nw"'
alias rmvol='rm /var/lib/order-connector/*'
alias pbin='pushd `git rev-parse --show-toplevel`/build/x86-64/debug/bin'
alias pext='pushd `git rev-parse --show-toplevel`/ext'
alias cdrt='cd `git rev-parse --show-toplevel`'
alias prt='pushd `git rev-parse --show-toplevel`'
alias default="cd ~/dev-root/default"
alias alternate="cd ~/dev-root/alternate"
alias edcfg='emacs -nw /etc/debesys/cme_oc_config.conf'
alias run='`git rev-parse --show-toplevel`/run'
alias ttknife='`git rev-parse --show-toplevel`/run `git rev-parse --show-toplevel`/ttknife'
alias envvers='knife environment list | xargs -n 1 -i knife environment show \{\} -a cookbook_versions'
alias lszk='`git rev-parse --show-toplevel`/run python `git rev-parse --show-toplevel`/darwin/python/lszk'
alias rmzk='`git rev-parse --show-toplevel`/run python `git rev-parse --show-toplevel`/darwin/python/rmzk'
alias oczk='lszk /srv/alive/oc -r; lszk /srv/oc -r | xargs --delimiter="\n" -n 1 echo "     "'
alias envs='echo PATH $PATH; echo LD_LIBRARY_PATH $LD_LIBRARY_PATH; echo C_INCLUDE_PATH $C_INCLUDE_PATH; echo CPLUS_INCLUDE_PATH $CPLUS_INCLUDE_PATH; echo PYTHONPATH $PYTHONPATH; echo PYTHONHOME $PYTHONHOME; echo SWIG_LIB $SWIG_LIB; echo DEBENV_ENGAGED $DEBENV_ENGAGED'

alias repo="python ~/githome/get-repo.py"
alias mdbd='sudo mount -o user=intad/tweiss -t cifs //chifs01.int.tt.local/Share/Dead_By_Dawn /mnt/dbd/'
alias cli_mt='run `git rev-parse --show-toplevel`/ext/linux/x86-64/release/bin/cli_mt 10.203.0.43:2181'
alias jtrader="/usr/java/jdk1.7.0_03/bin/java -cp JTrader.jar JTrader &"
alias ttr='`git rev-parse --show-toplevel`/run python `git rev-parse --show-toplevel`/t_trader/tt/ttrader/t_trader.py --stdout'
alias grp="git rev-parse --short"
alias myec2='aws ec2 describe-instances --region us-east-1 --filters "Name=tag-value,Values=tweiss"'
# alias chrome="/opt/google/chrome/google-chrome --enable-plugins &"

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
    elif [[ $1 == sy* || $1 == sg* ]]; then
        chef_config=~/.chef/knife.external.rb
    elif [[ $1 == *"ip-10-210-0"* || $1 == *"ip-10-210-2"* || $1 == *"ip-10-210-4"* ]]; then
        chef_config=~/.chef/knife.external.rb
    elif [[ $1 == *"ip-10-213-0"* || $1 == *"ip-10-213-2"* || $1 == *"ip-10-213-4"* ]]; then
        chef_config=~/.chef/knife.external.rb
    elif [[ $1 == *"ip-10-215-0"* || $1 == *"ip-10-215-2"* || $1 == *"ip-10-215-4"* ]]; then
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

function eaddtag2query()
{
    addtag2query "$1" "$2" ext
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
    echo knife node show "$1" --config $chef_config
    knife node show "$1" --config $chef_config
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
        alias ttknife='`git rev-parse --show-toplevel`/run `git rev-parse --show-toplevel`/ttknife -C ~/.chef/knife.external.rb'
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

        alias ttknife='`git rev-parse --show-toplevel`/run `git rev-parse --show-toplevel`/ttknife'
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
    `git rev-parse --show-toplevel`/run `git rev-parse --show-toplevel`/ttknife $config node delete --yes "$1"

    echo "ttknife $config client delete --yes $1"
    `git rev-parse --show-toplevel`/run `git rev-parse --show-toplevel`/ttknife $config client delete --yes "$1"
}

function search_chef_environment()
{
    local hm=~
    local config="-C $hm/.chef/knife.rb"
    if [ "$EXTERNAL_DEBESYS" == "enabled" ]; then
        config=" -C $hm/.chef/knife.external.rb"
    fi

    if [ -z "$1" ]; then
        echo Usage: You must pass the Chef environment and optionally a recipe.
        echo "Examples: sce int-dev-cert (all nodes in int-dev-cert)"
        echo "          sce int-dev-cert cme (all nodes in int-dev-cert with recipe cme)"
        return
    fi
    local search="chef_environment:$1"

    if [ ! -z "$2" ]; then
        search=$search" AND recipe:$2*"
    fi

    echo ttknife --config $config search node $search
    `git rev-parse --show-toplevel`/run `git rev-parse --show-toplevel`/ttknife $config search node "$search" -a name -a environment -a ipaddress -a run_list -a tags
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
    local ips=$($(git rev-parse --show-toplevel)/run $(git rev-parse --show-toplevel)/ttknife --config $knife_config search node "chef_environment:$1 AND recipe:$2*" -a ipaddress --no-color | grep ipaddress | tr -s " " | cut -d " " -f 3)

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

function deploy__()
{
    local found_dash_a=false
    # Example of bash substring match.
    if [[ "$@" == *"-a"* ]]; then
        found_dash_a=true
    fi

    local found_dash_h=false
    if [[ "$@" == *"-h"* ]]; then
        found_dash_h=true
    fi

    if [ $found_dash_h == false -a $found_dash_a == false ]; then
        local title_start="deploying..."
        local window=`tmux list-windows | grep "\(active\)" | cut -d" " -f 1 | sed s'/://g'`
        rename_terminal_title "$title_start"
    fi

    echo $DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/request_deploy.py "$@"
    $DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/request_deploy.py "$@"

    if [ $found_dash_h == false -a $found_dash_a == false ]; then
        local title_done="deploying...done"
        rename_terminal_title "$title_done" "$window"
    fi
}
alias deploy=deploy__

function bootstrap__()
{
    local found_dash_a=false
    # Example of bash substring match.
    if [[ "$@" == *"-a"* ]]; then
        found_dash_a=true
    fi

    local found_dash_h=false
    if [[ "$@" == *"-h"* ]]; then
        found_dash_h=true
    fi

    if [ $found_dash_h == false -a $found_dash_a == false ]; then
        local title_start="bootstrapping..."
        local window=`tmux list-windows | grep "\(active\)" | cut -d" " -f 1 | sed s'/://g'`
        rename_terminal_title "$title_start"
    fi

    echo $DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/request_bootstrap.py "$@"
    $DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/request_bootstrap.py "$@"

    if [ $found_dash_h == false -a $found_dash_a == false ]; then
        local title_done="bootstrapping...done"
        rename_terminal_title "$title_done" "$window"
    fi
}
alias bootstrap=bootstrap__

function build__()
{
    local found_dash_h=false
    if [[ "$@" == *"-h"* ]]; then
        found_dash_h=true
    fi

    local found_dash_a=false
    if [[ "$@" == *"-a"* ]]; then
        found_dash_a=true
    fi

    if [ $found_dash_h == false -a $found_dash_a == false ]; then
        local title_start="building..."
        local window=`tmux list-windows | grep "\(active\)" | cut -d" " -f 1 | sed s'/://g'`
        rename_terminal_title "$title_start"
    fi

    echo $DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/request_build.py "$@"
    $DEPLOYMENT_SCRIPTS_REPO_ROOT/run python $DEPLOYMENT_SCRIPTS_REPO_ROOT/deploy/chef/scripts/request_build.py "$@"

    if [ $found_dash_h == false -a $found_dash_a == false ]; then
        local title_done="building...done"
        rename_terminal_title "$title_done" "$window"
    fi
}
alias build=build__

# To view the definition of a function, do 'type <function>'.
function cf() { emacsclient -n `find . -name $1`; }
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

    local branch="$1"
    if [ "d" == "$1" ]; then
        branch="develop"
    elif [ "r" == "$1" ]; then
        branch="release/current"
    elif [ "u" == "$1" ]; then
        branch="uat/current"
    elif [ "m" == "$1" ]; then
        branch="master"
    fi

    echo "pushd `git rev-parse --show-toplevel`";
    pushd `git rev-parse --show-toplevel`;
    if [ $? != 0 ]; then
        echo "Aborting."
        echo "popd"; popd;
        return
    fi
    echo "git remote prune origin";
    git remote prune origin;
    if [ $? != 0 ]; then
        echo "Aborting."
        echo "popd"; popd;
        return
    fi
    echo "git checkout $branch";
    git checkout "$branch";
    if [ $? != 0 ]; then
        echo "Aborting."
        echo "popd"; popd;
        return
    fi
    echo "git pull";
    git pull;
    if [ $? != 0 ]; then
        echo "Aborting."
        echo "popd"; popd;
        return
    fi
    echo "git submodule init";
    git submodule init;
    if [ $? != 0 ]; then
        echo "Aborting."
        echo "popd"; popd;
        return
    fi
    echo "git submodule update";
    git submodule update;
    if [ $? != 0 ]; then
        echo "Aborting."
        echo "popd"; popd;
        return
    fi
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

    echo ./run python deploy/chef/scripts/ec2_server.py --size m1.medium --ami ami-eb6b0182 --dept debesys --cost-center "Engineering-490" --project debesys --manager "Tom Weiss" --user root --environment int-dev-sparepool --recipe base $ebs_size -a $1
    ./run python deploy/chef/scripts/ec2_server.py --size m1.medium --ami ami-eb6b0182 --dept debesys --cost-center "Engineering-490" --project debesys --manager "Tom Weiss" --user root --environment int-dev-sparepool --recipe base $ebs_size -a $1

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

    echo ./run python deploy/chef/scripts/ec2_server.py -d $1
    ./run python deploy/chef/scripts/ec2_server.py -d $1

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
    ./run python ./deploy/chef/scripts/upload_debesys.py --tag $1
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
if [ ! $TMUX_PANE ]; then
    rename_terminal_title ":-)"
fi

csview()
{
    local file="$1"
    cat "$file" | sed -e 's/,,/, ,/g' | column -s, -t | less -#5 -N -S
}


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

echo DISPLAY is $DISPLAY.
echo sudo netstat -tulpn | grep "127.0.0.0:60"
sudo netstat -tulpn | grep "127.0.0.0:60"
echo ps -ef | grep sshd | grep tweiss@
ps -ef | grep sshd | grep tweiss@
echo "Run killmyssh to kill all current sessions."
alias killmyssh='ps -ef | grep sshd | grep tweiss@ | tr -s " " | cut -d" " -f 2 | xargs kill'
# If I do 'pkill -u tweiss' that will kill all my sessions and get sshd back on display 10.