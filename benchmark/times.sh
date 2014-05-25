#!/bin/bash

# NB: should be launched from the project's root

scalaversion='2.11'

sourcepath="tests/src/*"
outputpath="benchmark/compiled/"
astpath="benchmark/asts/"
rawpath="benchmark/raw/"

astc_jar="plugin/target/scala-2.11/plugin-assembly-0.1.0-SNAPSHOT.jar"

# First, let's check the scalac versionb
ret="$(scalac -version 2>&1 1>/dev/null)"
if [[ $ret =~ $scalaversion ]]; then
	echo "Checking version of scalac: ok."
else
	echo "wrong version of scalac, got $ret, needed $scalaversion."
	exit
fi

# Let's first package the NSC plugin
sbt "plugin/assembly" > /dev/null 2>&1

# Let's create the temporary output directory
rm -r $outputpath > /dev/null 2>&1
mkdir $outputpath

# Forcing the time format
saved_TIMEFORMAT="$TIMEFORMAT"
TIMEFORMAT='%E'

# General variables
total_time_norm=0
total_time_astc=0
nb_tests=0

#iterate on all files from the sourcepath
for f in $(find $sourcepath -type f)
do
	nb_tests=$(echo "scale=0; $nb_tests + 1" | bc)

	# scalac options:
	# 	1. -Xplugin:<absolutepath>		Passing a NSC plugin
	# 	2. -d <directory|jar>			Specify output directory
	#	3. -classpath <path1:path2>		Specify the classpath for the compilation
	# Unfortunately for time precision reasons, we cannot use sbt dependency mechanism. We need to store the .jar specifically.
	# TODO: make that work with Typers.scala
	time_norm="$(time (scalac "$f" -d "$outputpath" -nowarn) 2>&1 1>/dev/null)"
	time_astc="$(time (scalac "$f" -Xplugin:"$astc_jar" -d "$outputpath" -nowarn) 2>&1 1>/dev/null)"

	# Let's test if numeric (in such a case, there should be no error)
	if [[ $time_norm =~ "error" ]]; then
		echo "Bad compilation for: $f"
	else
		if [[ $time_norm =~ "warning" ]]; then
			echo "Got warnings, unfortunately this would break our stats for file $f" # Should never happen due to -nowarn
		else
			time_incr=$(echo "scale=10; $time_astc / $time_norm" | bc)
			time_incr=$(echo "scale=10; $time_incr - 1" | bc)
			time_diff=$(echo "scale=10; $time_astc - $time_norm" | bc)
			total_time_norm=$(echo "scale=10; $total_time_norm + $time_norm" | bc)
			total_time_astc=$(echo "scale=10; $total_time_astc + $time_astc" | bc)

			echo "$nb_tests: normal time: $time_norm, with astc: $time_astc (+ $time_diff), increased of (ratio): $time_incr for file $f"
		fi
	fi
done

# Print general statistics ~~~~ #
echo "-------------------------"
echo "Total time with astc: $total_time_astc"
echo "Total time normally: $total_time_norm"
total_incr=$(echo "scale=5; $total_time_astc / $total_time_norm" | bc)
total_incr=$(echo "scale=5; $total_incr - 1" | bc)
echo "In average, the time is increased of (ratio): $total_incr"

# Let's cleanup the folders
rm -r $outputpath > /dev/null 2>&1
rm -r $astpath > /dev/null 2>&1
rm -r $rawpath > /dev/null 2>&1

# Restoring the time format
TIMEFORMAT="$saved_TIMEFORMAT"