#!/bin/bash

BASE="gitlab/minisafe"
PATTERN="(oracle.com|robama)"

for i in $*; do
	if ! git ls-files --error-unmatch $i &> /dev/null; then
		echo "$i: not known to git"
		continue
	fi

	count=`git rev-list -E --author=$PATTERN --count $BASE..HEAD -- $i`
	if (($count > 0)); then
		echo "$i: Oracle commits found"
		git log --pretty=format:"  Oracle author of %h was %an, %ar" -E --author=$PATTERN $BASE..HEAD -- $i
		safe-oracle-cpyrt.py $i
	fi
done
