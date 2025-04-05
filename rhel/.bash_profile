# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

# Right now there is a problem with settings in this file.  The trouble is that after login (GUI or
# ssh), this is executed.  However, when the terminal is opened in the GUI it runs .bashrc which
# executes /etc/bashrc and /etc/bashrc resets PATH (among others) so this is lost.  Once that is
# fixed PATHs can be set here.

PATH=$PATH:$HOME/bin

export PATH

# Commented this out since Ubuntu laptop has sudo prompt and I don't really need this all the time.
# if [ ! -f /var/log/profiles ]
# then
#     sudo touch /var/log/profiles
#     sudo chmod a+rw /var/log/profiles
# fi

# if [ -z "$SSH_AUTH_SOCK" ]; then
#     eval `ssh-agent -s`
#     ssh-add
# fi

RED='\033[91m'
BOLD='\033[1m'
ENDC='\033[0m'

if [[ -z "$TMUX" && -f ~/display_check ]]; then
    # The TMUX environment variable is not set, we're not inside tmux.
    if [[ $DISPLAY = *"10.0"* ]]; then
        echo DISPLAY is $DISPLAY.
    else
        echo -e "${RED}${BOLD}DISPLAY is $DISPLAY${ENDC}"
    fi
    echo sudo netstat -tulpn | grep "127.0.0.0:60"
    sudo netstat -tulpn | grep "127.0.0.0:60"
    echo ps -ef | grep sshd | grep tweiss@
    ps -ef | grep sshd | grep tweiss@
    echo "Run killmyssh to kill all current sessions."
fi

# On git 1.7.1 merges did not open the editor, but when I switched to 1.9.1 they do.
export GIT_MERGE_AUTOEDIT=no

# sudo yum -y install centos-release-scl
# sudo yum -y install git19
# scl enable git19 bash
if [ -f /opt/rh/git19/enable ]; then
    source /opt/rh/git19/enable
fi

if [ -f /opt/rh/rh-git218/enable ]; then
    source /opt/rh/rh-git218/enable
fi

# Commented this out since Ubuntu laptop has sudo prompt.
# sudo echo .bash_profile ran at $(date) >> /var/log/profiles
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# For setting machine specific environment settings.
if [[ -f ~/environment ]]; then
    source ~/environment
fi

if [[ -n "${CD_HERE}" ]]; then
    cd ${CD_HERE}
fi

# Created by `pipx` on 2021-05-26 15:07:17
export PATH="$PATH:/home/tweiss/.local/bin"

# if [[ $HOSTNAME == gld* ]]; then
#     # from Axe Setup
#     eval "$(direnv hook bash)"
# fi

tmux a -d
