

usage="mklinks.sh [--force]"
export mklinks_force_delete="off"
if [ "--force" == "$1" ]; then
    export mklinks_force_delete="on"
fi

# The reason to print and sleep if --force is enabled is that I typically only run this script when
# I'm setting up a new workstation (not too often).  I'm typically in a hurry and don't always
# remember how this script works.
if [ "on" == $mklinks_force_delete ]; then
    echo "The --force option is on, all files will be over written (sleeping 5 seconds to allow abort)."
    sleep 5
else
    echo "The --force option is off, existing files will not be over written."
fi


# The command to create a soft link in Windows is: mklink link target.

pushd $HOME

if [ -d /etc/sbt ]; then  # If directory exists, then it's likely sbt is installed
    if [ ! -f /etc/sbt/sbtopts ]; then
        sudo ln -sv $HOME/githome/sbtopts /etc/sbt/sbtopts
    elif [ "on" == $mklinks_force_delete ]; then
        echo Deleting /etc/sbt/sbtopts and re-creating since --force was enabled.
        sudo rm -v /etc/sbt/sbtopts
        sudo ln -sv $HOME/githome/sbtopts /etc/sbt/sbtopts
    else
        echo Skipping $HOME/githome/sbtopts since /etc/sbt/sbtopts already exists.
    fi
else
    echo Skipping $HOME/githome/sbtopts since sbt does not seem to be installed.
fi

# Files in home directory whose link name is identical.
for f in .bash_profile .bashrc .inputrc .tcshrc
do
    if [ ! -f $HOME/$f ]; then
        ln -sv $HOME/githome/rhel/$f $HOME/$f
    elif [ "on" == $mklinks_force_delete ]; then
        echo Deleting $HOME/$f and re-creating since --force was enabled.
        rm -v $HOME/$f
        ln -sv $HOME/githome/rhel/$f $HOME/$f
    else
        echo Skipping $HOME/$f, already exists.
    fi
done


# Files in $HOME/ from githome.
for f in .emacs .tmux.conf
do
    if [ ! -f $HOME/$f ]; then
        ln -sv $HOME/githome/$f $HOME/$f
    elif [ "on" == $mklinks_force_delete ]; then
        echo Deleting $HOME/$f and re-creating since --force was enabled.
        rm -v $HOME/$f
        ln -sv $HOME/githome/$f $HOME/$f
    else
        echo Skipping $HOME/$f, already exists.
    fi
done


# Special case files which have a different name.
if [ ! -f $HOME/.gitconfig ]; then
    ln -sv $HOME/githome/rhel/dotgitconfig $HOME/.gitconfig
elif [ "on" == $mklinks_force_delete ]; then
    echo Deleting $HOME/.gitconfig and re-creating since --force was enabled.
    rm -v $HOME/.gitconfig
    ln -sv $HOME/githome/rhel/dotgitconfig $HOME/.gitconfig
else
    echo Skipping $HOME/.gitconfig, already exists.
fi

if [ ! -f $HOME/.gitignore_global ]; then
    ln -sv $HOME/githome/rhel/dotgitignore_global $HOME/.gitignore_global
elif [ "on" == $mklinks_force_delete ]; then
    echo Deleting $HOME/.gitignore_global and re-creating since --force was enabled.
    rm -v $HOME/.gitignore_global
    ln -sv $HOME/githome/rhel/dotgitignore_global $HOME/.gitignore_global
else
    echo Skipping $HOME/.gitignore_global, already exists.
fi

if [ ! -f $HOME/.ssh/config ]; then
    ln -sv $HOME/githome/dotsshconfig $HOME/.ssh/config
elif [ "on" == $mklinks_force_delete ]; then
    echo Deleting $HOME/.ssh/config and re-creating since --force was enabled.
    rm -v $HOME/.ssh/config
    ln -sv $HOME/githome/dotsshconfig $HOME/.ssh/config
else
    echo Skipping $HOME/.ssh/config, already exists.
fi


# Files in $HOME/.emacs.d whose link name is identical.
mkdir -pv $HOME/.emacs.d
for f in custom-compile.el custom-electric-buffer-list.el custom-file-cache.el custom-find-grep.el custom-gdb-functions.el custom-tags.el cygwin-mount.el find-and-open-file.el find-best-root.el mks-commands.el windows-path.el chef-helpers.el web-mode.el
do
    if [ ! -f $HOME/.emacs.d/$f ]; then
        ln -sv $HOME/githome/$f $HOME/.emacs.d/$f
    elif [ "on" == $mklinks_force_delete ]; then
        echo Deleting $HOME/.emacs.d/$f and re-creating since --force was enabled.
        rm -v $HOME/.emacs.d/$f
        ln -sv $HOME/githome/$f $HOME/.emacs.d/$f
    else
        echo Skipping $HOME/$f, already exists.
    fi
done


# Files in $HOME/githome/scala-mode.
mkdir -pv $HOME/.emacs.d/scala-mode
for f in scala-mode-feature.el scala-mode-fontlock.el scala-mode-navigation.el scala-mode-auto.el scala-mode-feature-electric.el scala-mode-indent.el scala-mode-ui.el scala-mode-constants.el scala-mode-feature-speedbar.el scala-mode-inf.el scala-mode-variables.el scala-mode.el scala-mode-feature-tags.el scala-mode-lib.el
do
    if [ ! -f $HOME/.emacs.d/scala-mode/$f ]; then
        ln -sv $HOME/githome/scala-mode/$f $HOME/.emacs.d/scala-mode/$f
    elif [ "on" == $mklinks_force_delete ]; then
        echo Deleting $HOME/githome/scala-mode/$f and re-creating since --force was enabled.
        rm -v $HOME/githome/scala-mode/$f
        ln -sv $HOME/githome/scala-mode/$f $HOME/.emacs.d/scala-mode/$f
    else
        echo Skipping $HOME/.emacs.d/scala-mode/$f, already exists.
    fi
done

# Gnome Settings.
gnome_profile=$HOME/.gconf/apps/gnome-terminal/profiles/Default/%gconf.xml
mkdir -pv $HOME/.gconf/apps/gnome-terminal/profiles/Default
if [ ! -f $gnome_profile ]; then
    ln -sv $HOME/githome/gnome-terminal-Default-profile-%gconf.xml $gnome_profile
    echo Logout for this to take effect. # Reminder to my impatient self.
elif [ "on" == $mklinks_force_delete ]; then
    echo Deleting $gnome_profile and re-creating since --force was enabled.
    rm -v $gnome_profile
    ln -sv $HOME/githome/gnome-terminal-Default-profile-%gconf.xml $gnome_profile
    echo Logout for this to take effect. # Reminder to my impatient self.
else
    echo Skipping $gnome_profile, already exists.
fi

gnome_keybindings=$HOME/.gconf/apps/gnome-terminal/keybindings/%gconf.xml
mkdir -pv $HOME/.gconf/apps/gnome-terminal/keybindings
if [ ! -f $gnome_keybindings ]; then
    ln -sv $HOME/githome/gnome-terminal-keybindings-%gconf.xml $gnome_keybindings
    echo Logout for this to take effect. # Reminder to my impatient self.
elif [ "on" == $mklinks_force_delete ]; then
    echo Deleting $gnome_keybindings and re-creating since --force was enabled.
    rm -v $gnome_keybindings
    ln -sv $HOME/githome/gnome-terminal-keybindings-%gconf.xml $gnome_keybindings
    echo Logout for this to take effect. # Reminder to my impatient self.
else
    echo Skipping $gnome_keybindings, already exists.
fi

popd
