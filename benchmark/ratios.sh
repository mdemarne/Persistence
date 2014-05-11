#!/bin/sh

# NB: should be launched from the project's root

scalaversion='2.11'
rawfolder="showraw/*"
ourfolder="asts/*"

# Launch the tests (will output the raw and the asts).
sbt "tests/clean"
sbt "tests/compile"

# Do benchmark for all files compiled
echo "All sizes are in bytes"
for f in $rawfolder
do
	sed -i 's/"[^"]*"/x/g' $f
	lzma $f -k # compress

	# Check compression ration between:
	#  1. The output uncompressed of showRaw and ours
	#  2. The output compressed of show and ours
	raw_size=$(stat -c %s $f)
	comp_size=$(stat -c %s "$f.lzma")

	our_path1=${f%.txt}
	our_path2=${our_path1##*/}
	our_size=$(stat -c %s "asts/$our_path2.ast")

	lzma_ratio=$(echo "scale=5; $comp_size / $raw_size" | bc)
	our_ratio=$(echo "scale=5; $our_size/ $raw_size" | bc)
	lzma_our_ratio=$(echo "scale=5; $our_size/ $comp_size" | bc)

	echo "For file $our_path2,\t showRaw: $raw_size,\t lzma: $comp_size ($lzma_ratio),\t ours: $our_size ($our_ratio),\t ratio of ratios (ours/lzma): $lzma_our_ratio"
	if [ $comp_size -lt $our_size ]; then echo "FAILED: Lzma better than ours."; fi
done

# Cleanup the test folders
rm $rawfolder -r
rm $ourfolder -r