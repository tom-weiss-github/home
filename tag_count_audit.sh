#!/bin/bash

git tag -d $(git tag) > /dev/null 2>&1 && git fetch
echo Tag count is `git tag | wc -l` at `date`.
