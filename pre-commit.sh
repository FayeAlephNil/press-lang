#!/bin/bash

hasChanges=$(git diff)
if [ -n "$hasChanges" ]; then
    git stash push --keep-index
fi

testResults=$(racket basic-tests.rkt 2>&1)
testSuccess=1

if [ -z "$testResults" ]; then
    testSuccess=0
fi

if [ -n "$hasChanges" ]; then
    git stash pop
fi

echo $testResults >&2
exit $testSuccess
