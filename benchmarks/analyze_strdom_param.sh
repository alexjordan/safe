#!/bin/bash -e

source analyze.inc.sh
if
 [[ "$3" == 'def' ]]
then
  sizeopt=''
else
  sizeopt="--max-strset-size $3"
fi
if
  [[ $1 == *".html"* ]]
then
  options="--benchmark --timeout 600 --no-imprecision-log -q --strdom $2 $sizeopt --html "
else
  options="--benchmark --timeout 600 --no-imprecision-log --no-imprecision-stop -q --strdom $2 $sizeopt --test "
  echo "neosafe $options"
fi

testsuffix='_'$2'_'$3

#######################################
# Script Code
#######################################

# Get this script directory
scriptDir=$PWD/$0
scriptDir="${scriptDir%/*}"/

timeout=600
inFiles=$1

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
				runAnalysis $i $tokenname
				
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
mv 

# End
echo "* All finished!"
