#!/bin/bash

# Execute as . ./trigger-merge-conflict.sh


function write_contents()
{
    if [ -z "$1" ]; then
        echo usage: you must pass the host alias
        return
    fi

    local file_name=$1
    local conflict_identifier=$2

    echo "This line (1) was added, but will be the same." >> $file_name
    echo "This line (2) was added, but will be the same." >> $file_name
    echo "This line (3) was added, but will be the same." >> $file_name
    echo "This line will be a conflict due to $conflict_identifier." >> $file_name
    echo "" >> $file_name

    echo "This line (1) was added, but will be the same." >> $file_name
    echo "This line (2) was added, but will be the same." >> $file_name
    echo "This line (3) was added, but will be the same." >> $file_name
    echo "This line will be a conflict due to $conflict_identifier." >> $file_name
    echo "" >> $file_name

    echo "This line (1) was added, but will be the same." >> $file_name
    echo "This line (2) was added, but will be the same." >> $file_name
    echo "This line (3) was added, but will be the same." >> $file_name
    echo "This line will be a conflict due to $conflict_identifier." >> $file_name
    echo "" >> $file_name
}

git checkout master
git branch -D conflict-test-alpha
git branch -D conflict-test-beta

git checkout -b conflict-test-alpha
write_contents "./conflict-test-1.txt" "alpha"
write_contents "./conflict-test-2.txt" "alpha"
git add conflict-test-1.txt conflict-test-2.txt
git commit -m "conflict-test-alpha"

git checkout master

git checkout -b conflict-test-beta
write_contents "./conflict-test-1.txt" "beta"
write_contents "./conflict-test-2.txt" "beta"
git add conflict-test-1.txt conflict-test-2.txt
git commit -m "conflict-test-beta"

git merge conflict-test-alpha
# (merge conflict)
# git reset --merge (to abort the merge)
# gits master

