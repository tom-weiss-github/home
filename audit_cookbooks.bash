#!/bin/bash

# Remove all existing files.
for deleteme in commits.txt commits_no_bumps.txt commits_no_merges.txt commits_authors.txt
do
    rm -v /tmp/$deleteme
done

git log --date-order --pretty=format:'%h %s ____%an___' --date=relative --since='12 month' -- deploy/chef/cookbooks > /tmp/commits.txt

cat /tmp/commits.txt | grep -vi 'bump cookbook versions' > /tmp/commits_no_bumps.txt

cat /tmp/commits_no_bumps.txt | grep -vi 'merge branch' > /tmp/commits_no_merges.txt

cat /tmp/commits_no_merges.txt | sed "s/^.*____/___/g" | sort > /tmp/commits_authors.txt