#!/bin/bash

# Execute as . ./trigger-merge-conflict.sh


function write_contents()
{
    if [ -z "$1" ]; then
        echo usage: you must pass the file
        return
    fi

    local file_name=$1

    for blocks in 1 2 3
    do
        for index in 1 2 3 4 5
        do
            echo "This line ($index) was added, but will be the same." >> $file_name
        done
        openssl rand -base64 32 >> $file_name
        echo "" >> $file_name
    done
}

git checkout master
git branch -D conflict-test-alpha
git branch -D conflict-test-beta

git checkout -b conflict-test-alpha
write_contents "./conflict-test-1.txt"
write_contents "./conflict-test-2.txt"
git add -v conflict-test-1.txt conflict-test-2.txt
git commit -m "conflict-test-alpha"

git checkout master

git checkout -b conflict-test-beta
write_contents "./conflict-test-1.txt"
write_contents "./conflict-test-2.txt"
git add -v conflict-test-1.txt conflict-test-2.txt
git commit -m "conflict-test-beta"

git merge conflict-test-alpha
# (merge conflict)
# git reset --merge (to abort the merge)
# gits master

