#!/bin/bash

git branch -d conflict-test-1
git branch -d conflict-test-2
git checkout -b conflict-test-1
echo conflict-1 > conflict-test.txt
git add conflict-test.txt
git commit -m "conflict-test-1"
git checkout master
git checkout -b conflict-test-2
echo conflict-2 > conflict-test.txt
git add conflict-test.txt
git commit -m "conflict-test-2"
git merge conflict-1
# (merge conflict)
# May need to manually delete the branches when done.