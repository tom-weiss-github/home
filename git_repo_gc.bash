#!/bin/bash

for repo_dir in $(ls -d ~/dev-root/*)
do
    cd $repo_dir
    if [[ -d .git ]]; then
        echo "Running 'git gc' in $repo_dir."
        logger -t gitgc -s "Running 'git gc' in $repo_dir."
        git gc

        echo "Running 'git remote prune origin' in $repo_dir."
        logger -t gitgc -s "Running 'git remote prune origin' in $repo_dir."
        git remote prune origin

        echo "Running 'git fetch' in $repo_dir."
        logger -t gitgc -s "Running 'git fetch' in $repo_dir."
        git fetch
        git status
    fi
done

echo "Finished Git Cleanup"
logger -t gitgc -s "Finished Git Cleanup"
