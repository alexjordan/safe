#######################################
# Settings
#######################################
#command="-cp ../../dist/tajs-all.jar dk.brics.tajs.MainJQueryDeterminacy"
command=""
#options="-domprop -timeout 600 -max-strset-size 30 -forin-unroll 1 -locclone -context-loop -disableEvent -newEvent"
options=
#domstatoption="-domstatistics DOMAPI.csv"
domstatoption=""
#domstatout="domAPI.txt"
domstatout=""
target="main_pretty.htm"
testsuffix="_normal"
threads=1
#######################################

websiteCount=0
websiteNames=
websiteDirs=

# Run function
function runAnalysis {
	i=$1
	tokenname=$2
	websiteName=${websiteNames[$i]}
	websiteDir=${websiteDirs[$i]}
	websiteHTML=$websiteDir/$target

	echo "*" Analyzing"("$(($i + 1))/$WebsiteCount")"... $websiteName
	neosafe $command $options $websiteName > $websiteName$testsuffix.txt
	echo "*" Finished! $websiteName

	touch $tokenname

	exit
}

