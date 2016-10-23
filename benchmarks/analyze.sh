#!/bin/bash -e

source analyze.inc.sh

options="--benchmark -q"

#######################################
# Script Code
#######################################

# Get this script directory
scriptDir=$PWD/$0
scriptDir="${scriptDir%/*}"/

inFiles=
if [[ -n $* ]]; then
	inFiles="$*"
else
	inFiles=`ls -d *.html`
fi

# Collect website directories
#for directory in `ls -d *.html`
for directory in $inFiles
do
	websiteNames[$WebsiteCount]=$directory
	((WebsiteCount++)) && true
done
echo "* Website count: "$WebsiteCount

# Create tokens
for (( i = 1 ; i <= threads ; i++ ))
do
	touch $scriptDir"token"$i".temp"
done

# For each website
for i in ${!websiteNames[*]}
do
	run=1
	while (( run == 1 ))
	do
		for (( j = 1 ; j <= $threads ; j++ ))
		do
			tokenname=$scriptDir"token"$j".temp"
			if [ -f $tokenname ]
			then
				rm $tokenname
				runAnalysis $i $tokenname &
				run=0
				break
			fi
		done
		# sleep 1 second if there is no available token
		if (( run == 1 )); then sleep 1; fi
	done
done

# Wait
run=1
while (( run == 1 ))
do
	run=0
	for (( i = 1 ; i <= threads ; i++ ))
	do
		tokenname=$scriptDir"token"$i".temp"
		if [ ! -f $tokenname ]
		then
			run=1
		fi
	done
	if (( run == 1 )); then sleep 1; fi
done

# Delete tokens
rm -f "$scriptDir"token*.temp

# End
echo "* All finished!"
