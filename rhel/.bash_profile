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

if [ ! -f /var/log/profiles ]
then
    sudo touch /var/log/profiles
    sudo chmod a+rw /var/log/profiles
fi

if [ -z "$SSH_AUTH_SOCK" ]; then
    eval `ssh-agent -s`
    ssh-add
fi

echo DISPLAY is $DISPLAY.
echo sudo netstat -tulpn | grep "127.0.0.0:60"
sudo netstat -tulpn | grep "127.0.0.0:60"
echo ps -ef | grep sshd | grep tweiss@
ps -ef | grep sshd | grep tweiss@
echo "Run killmyssh to kill all current sessions."

sudo echo .bash_profile ran at $(date) >> /var/log/profiles
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
