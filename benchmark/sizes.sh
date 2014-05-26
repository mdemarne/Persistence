#!/bin/bash

# NB: should be launched from the project's root

scalaversion='2.11'
rawfolder="tests/target/scala-$scalaversion/raw/*"
astfolder="tests/target/scala-$scalaversion/asts"

# data file for plots
date=$(date +%Y.%m.%d:%H:%M:%S)
datafile="benchmark/plots/plot-$date.dat"
mkdir "benchmark/plots/" > /dev/null 2>&1 # Avoiding error if the folder didn't exist before.
echo '# normal_size		astc_size 		xz_size' >> $datafile

# Allow to select which test we would like to run easily.
case $1 in
	1) conf="testBasic" ;;
	2) conf="testScalalib" ;;
	3) conf="testTypers" ;;
	*) echo "Arguments: 1 for testBasic, 2 for testScalalib, 3 for testTypers."; exit ;;
esac

# Generate good SBT commands
compile=":compile"
noplug="NoPlug"
confCompile=$conf$compile
confNoPlugCompile=$conf$noplug$compile

# Let's first clean the folder (if required)
rm $rawfolder -r > /dev/null 2>&1

# Now let's clean SBT
sbt "tests/clean" > /dev/null 2>&1
# Let first mesure the time ~~~~ #

# Perhaps a way using input task to have iteration on file to test them separately.

normal_time="$(time (sbt $confNoPlugCompile) 2>&1 1>/dev/null)" # Compute the normal time of compilation
astc_time="$(time (sbt $confCompile) 2>&1 1>/dev/null)" # Time with our plugin

# Do benchmark for all files compiled
echo "All sizes are in bytes"

total_raw_size=0
total_xz_size=0
total_astc_size=0
nb_tests=0
nb_failed=0

for f in $(find $rawfolder -type f)
do
	nb_tests=$(echo "scale=0; $nb_tests + 1" | bc)

	# For benchmark correctness, we simplify the showRaw as much as possible using simple substitutions ~~~~ #

	# Replacing user-defined names
	sed -i 's/"[^"]*"/x/g' $f

	# Replacing all the names of ASTs by simple character. Since this is a simple optimization, we do it for correctness
	sed -i 's/PackageDef/a/g' $f
	sed -i 's/ClassDef/b/g' $f
	sed -i 's/ModulDef/c/g' $f
	sed -i 's/ValDef/d/g' $f
	sed -i 's/DefDef/e/g' $f
	sed -i 's/TypeDef/f/g' $f
	sed -i 's/LabelDef/g/g' $f
	sed -i 's/Import/h/g' $f
	sed -i 's/Template/i/g' $f
	sed -i 's/Block/j/g' $f
	sed -i 's/CaseDef/k/g' $f
	sed -i 's/Alternative/l/g' $f
	sed -i 's/Star/m/g' $f
	sed -i 's/Bind/n/g' $f
	sed -i 's/UnApply/o/g' $f
	sed -i 's/ArrayValue/p/g' $f
	sed -i 's/Function/q/g' $f
	sed -i 's/Assign/r/g' $f
	sed -i 's/AssignOrNamedArg/s/g' $f
	sed -i 's/If/t/g' $f
	sed -i 's/Match/u/g' $f
	sed -i 's/Return/v/g' $f
	sed -i 's/Try/w/g' $f
	sed -i 's/Throw/y/g' $f
	sed -i 's/New/z/g' $f
	sed -i 's/Typed/A/g' $f
	sed -i 's/TypeApply/B/g' $f
	sed -i 's/Apply/C/g' $f
	sed -i 's/ApplyDynamic/D/g' $f
	sed -i 's/This/E/g' $f
	sed -i 's/Select/F/g' $f
	sed -i 's/Ident/G/g' $f
	sed -i 's/ReferenceToBoxed/H/g' $f
	sed -i 's/Literal/I/g' $f
	sed -i 's/Annotated/J/g' $f
	sed -i 's/SingletonTypeTree/K/g' $f
	sed -i 's/SelectFromTypeTree/L/g' $f
	sed -i 's/CompoundTypeTree/M/g' $f
	sed -i 's/AppliedTypeTree/N/g' $f
	sed -i 's/TypeBoundsTree/O/g' $f
	sed -i 's/ExistentialTypeTree/P/g' $f
	sed -i 's/TypeTree/Q/g' $f
	sed -i 's/Super/R/g' $f

	# Replacing Terms, modifiers and other unstored things
	sed -i 's/TermName//g' $f # We don't store names for now, let's remove them for correctness
	sed -i 's/TypeName//g' $f # We don't store names for now, let's remove them for correctness
	sed -i 's/Modifiers([^"(]*)//g' $f # We don't store modifiers for now, let's remove them for correctness
	sed -i 's/List/S/g' $f
	sed -i 's/noSelfType//g' $f # We don't store types for now, let's remove them for correctness
	sed -i 's/Constant//g' $f # We don't store constants for now, let's remove them for correctness
	sed -i 's/typeNames.EMPTY//g' $f # We don't store types for now, let's remove them for correctness
	sed -i 's/typeNames.CONSTRUCTOR//g' $f # We don't store types for now, let's remove them for correctness
	sed -i 's/termNames.EMPTY//g' $f # We don't store names for now, let's remove them for correctness
	sed -i 's/termNames.CONSTRUCTOR//g' $f # We don't store names for now, let's remove them for correctness
	sed -i 's/.setOriginal/T/g' $f


	# Let's now compute the sizes and ratios ~~~~ #

	xz $f -k # compress using xz, as we do in the second pass of our own algorithm.

	# Check compression ration between:
	#  1. The output uncompressed of showRaw and astcs
	#  2. The output compressed of show and astcs
	raw_size=$(stat -c %s $f)
	xz_size=$(stat -c %s "$f.xz")

	astc_path1=${f%.raw}
	astc_path2=${astc_path1#*/*/*/*/} # Cleanup the path from raw to get asts
	astc_size=$(stat -c %s "$astfolder/$astc_path2.ast")

	xz_ratio=$(echo "scale=5; $xz_size / $raw_size" | bc)
	astc_ratio=$(echo "scale=5; $astc_size / $raw_size" | bc)
	xz_astc_ratio=$(echo "scale=5; $astc_size / $xz_size" | bc)

	total_raw_size=$(echo "scale=5; $total_raw_size + $raw_size" | bc)
	total_xz_size=$(echo "scale=5; $total_xz_size + $xz_size" | bc)
	total_astc_size=$(echo "scale=5; $total_astc_size + $astc_size" | bc)

	# Let's print the results for a single file ~~~~ #

	echo "showRaw: $raw_size, xz: $xz_size ($xz_ratio), astc: $astc_size ($astc_ratio), ratio of ratios (astc/xz): $xz_astc_ratio  for file $astc_path2"
	if [ $xz_size -lt $astc_size ]; then 
		nb_failed=$(echo "scale=0; $nb_failed + 1" | bc)
		echo "FAILED: xz better than astc."
	fi

	# Output the results to the datafile for plots ~~~~ #
	echo "$raw_size  	$astc_size 		$xz_size" >> $datafile
done

# Print the global statistics ~~~~ #

echo "Global statistics"
echo "Raw: $total_raw_size, xz: $total_xz_size, astc: $total_astc_size"
total_xz_astc_ratio=$(echo "scale=5; $total_astc_size / $total_xz_size" | bc)
echo "In general, our compression is smaller than a classic xz of $total_xz_astc_ratio"
echo "Tests where xz was better: $nb_failed over $nb_tests tests"

echo "The normal compilation time was of:"
echo $normal_time
echo "The time of compilation using the plugin was of:"
echo $astc_time


# Let's now plot everything
gnuplot -p -e "plot \"$datafile\" using 1:2 title 'astc', \"$datafile\" using 1:3 title 'xz'"