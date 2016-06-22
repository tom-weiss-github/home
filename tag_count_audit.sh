#!/bin/bash

while true
do
    git tag -d $(git tag) > /dev/null 2>&1 && git fetch > /dev/null 2>&1
    echo Tag count is `git tag | wc -l` at `date`.
    sleep 300
done
