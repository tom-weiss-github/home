#!/bin/bash

# Execute as . ./trigger-merge-conflict.sh

function write_contents()
{
    if [ -z "$1" ]; then
        echo usage: you must pass the file
        return
    fi

    local file_name=$1
    truncate -s 0 $file_name

    for blocks in 1 2 3
    do
        for index in 1 2 3 4 5 6 7 8 9 10
        do
            echo "This line ($blocks)-($index) but will be the same in both files." >> $file_name
        done
        openssl rand -base64 32 >> $file_name
        echo "" >> $file_name
    done
}

# This code was run once to create the files with content so that they have a common
# ancestor with similar content.  I found if I just create files which are similar
# on different branches then the entire file shows as a diff.  This will only need
# to be done again if the write_contents function changes.
# write_contents "./conflict-test-1.txt"
# write_contents "./conflict-test-2.txt"
# return

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

