#!/bin/bash

for rel in 1 2 3 4 5
do
    branch=rel # For master, just replace branch with master.

    echo git checkout $branch
    git checkout $branch

    echo git pull --quiet
    git pull --quiet

    echo git submodule update --quiet
    git submodule update --quiet

    echo git config -f .git/config --remove-section submodule.client.java/SlidingMenu
    git config -f .git/config --remove-section submodule.client.java/SlidingMenu

    echo git config -f .gitmodules --remove-section submodule.client.java/SlidingMenu
    git config -f .gitmodules --remove-section submodule.client.java/SlidingMenu

    echo git add -v .gitmodules
    git add -v .gitmodules

    echo git rm --cached client.java/SlidingMenu
    git rm --cached client.java/SlidingMenu

    echo git commit -m "remove sliding menu submodule @goodgardening"
    git commit -m "remove sliding menu submodule @goodgardening"

    echo git push origin $branch
    git push origin $branch

    echo ""
    echo git submodule status client.java
    git submodule status client.java
    echo ""
done
