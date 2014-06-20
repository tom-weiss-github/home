#!/bin/bash

# Execute as . ./trigger-merge-conflict.sh

git branch -D conflict-test-1
git branch -D conflict-test-2
git checkout -b conflict-test-1
echo conflict-1 >> conflict-test.txt
git add conflict-test.txt
git commit -m "conflict-test-1"
git checkout master
git checkout -b conflict-test-2
echo conflict-2 >> conflict-test.txt
git add conflict-test.txt
git commit -m "conflict-test-2"
git merge conflict-test-1
# (merge conflict)
# git reset --merge (to abort the merge)
# gits master
