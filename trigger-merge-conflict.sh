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

    echo "This line (1) was added, but will be the same." >> $file_name
    echo "This line (2) was added, but will be the same." >> $file_name
    echo "This line (3) was added, but will be the same." >> $file_name
    echo "This line will be a conflict due to $conflict_identifier." >> $file_name

    echo "This line (1) was added, but will be the same." >> $file_name
    echo "This line (2) was added, but will be the same." >> $file_name
    echo "This line (3) was added, but will be the same." >> $file_name
    echo "This line will be a conflict due to $conflict_identifier." >> $file_name
}

git branch -D conflict-test-1
git branch -D conflict-test-2
git checkout -b conflict-test-1
write_contents "./conflict-test-1.txt" "1"
git add conflict-test.txt
git commit -m "conflict-test-1"
git checkout master
git checkout -b conflict-test-2
write_contents "./conflict-test-2.txt" "2"
git add conflict-test.txt
git commit -m "conflict-test-2"
git merge conflict-test-1
# (merge conflict)
# git reset --merge (to abort the merge)
# gits master

