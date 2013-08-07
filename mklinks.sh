
pushd $HOME

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
    ln -sv $HOME/githome/rhel/dotgitconfig $HOME/dotgitignore_global
else
    echo Skipping $HOME/.gitignore_global, already exists.
fi


# Files in $HOME/.emacs.d whose link name is identical.
mkdir -pv $HOME/.emacs.d
for f in custom-compile.el custom-electric-buffer-list.el custom-file-cache.el custom-find-grep.el custom-gdb-functions.el custom-tags.el cygwin-mount.el find-and-open-file.el find-best-root.el mks-commands.el windows-path.el
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

popd