
# The command to create a soft link in Windows is: mklink link target.

pushd $HOME

if [ -d /etc/sbt ]; then  # If directory exists, then it's likely sbt is installed
    if [ ! -f /etc/sbt/sbtopts ]; then
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
    else
        echo Skipping $HOME/$f, already exists.
    fi
done


# Files in $HOME/ from githome.
for f in .emacs
do
    if [ ! -f $HOME/$f ]; then
        ln -sv $HOME/githome/$f $HOME/$f
    else
        echo Skipping $HOME/$f, already exists.
    fi
done


# Special case files which have a different name.
if [ ! -f $HOME/.gitconfig ]; then
    ln -sv $HOME/githome/rhel/dotgitconfig $HOME/.gitconfig
else
    echo Skipping $HOME/.gitconfig, already exists.
fi

if [ ! -f $HOME/.gitignore_global ]; then
    ln -sv $HOME/githome/rhel/dotgitignore_global $HOME/.gitignore_global
else
    echo Skipping $HOME/.gitignore_global, already exists.
fi


# Files in $HOME/.emacs.d whose link name is identical.
mkdir -pv $HOME/.emacs.d
for f in custom-compile.el custom-electric-buffer-list.el custom-file-cache.el custom-find-grep.el custom-gdb-functions.el custom-tags.el cygwin-mount.el find-and-open-file.el find-best-root.el mks-commands.el windows-path.el chef-helpers.el
do
    if [ ! -f $HOME/.emacs.d/$f ]; then
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
    else
        echo Skipping $HOME/.emacs.d/scala-mode/$f, already exists.
    fi
done

# Gnome Settings.
gnome_profile=$HOME/.gconf/apps/gnome-terminal/profiles/Default/%gconf.xml
if [ ! -f $gnome_profile ]; then
    ln -sv $HOME/githome/gnome-terminal-Default-profile-%gconf.xml $gnome_profile
    echo Logout for this to take effect. # Reminder to my impatient self.
else
    echo Skipping $gnome_profile, already exists.
fi

gnome_keybindings=$HOME/.gconf/apps/gnome-terminal/keybindings/%gconf.xml
if [ ! -f $gnome_keybindings ]; then
    ln -sv $HOME/githome/gnome-terminal-keybindings-%gconf.xml $gnome_keybindings
    echo Logout for this to take effect. # Reminder to my impatient sel.f
else
    echo Skipping $gnome_keybindings, already exists.
fi

popd