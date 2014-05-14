#!/bin/sh

# NB: should be launched from the project's root

scalaversion='2.11'
rawfolder="showraw/*"
astcfolder="asts/*"

# Launch the tests (will output the raw and the asts).
sbt "tests/clean"
sbt "tests/compile"

# Do benchmark for all files compiled
echo "All sizes are in bytes"

total_raw_size=0
total_lzma_size=0
total_astc_size=0

for f in $rawfolder
do
	sed -i 's/"[^"]*"/x/g' $f
	lzma $f -k # compress

	# Check compression ration between:
	#  1. The output uncompressed of showRaw and astcs
	#  2. The output compressed of show and astcs
	raw_size=$(stat -c %s $f)
	lzma_size=$(stat -c %s "$f.lzma")

	astc_path1=${f%.txt}
	astc_path2=${astc_path1##*/}
	astc_size=$(stat -c %s "asts/$astc_path2.ast")

	lzma_ratio=$(echo "scale=5; $lzma_size / $raw_size" | bc)
	astc_ratio=$(echo "scale=5; $astc_size / $raw_size" | bc)
	lzma_astc_ratio=$(echo "scale=5; $astc_size / $lzma_size" | bc)

	total_raw_size=$(echo "scale=5; $total_raw_size + $raw_size" | bc)
	total_lzma_size=$(echo "scale=5; $total_lzma_size + $lzma_size" | bc)
	total_astc_size=$(echo "scale=5; $total_astc_size + $astc_size" | bc)

	echo "For file $astc_path2,\t showRaw: $raw_size,\t lzma: $astc_size ($lzma_ratio),\t astc: $astc_size ($astc_ratio),\t ratio of ratios (astc/lzma): $lzma_astc_ratio"
	if [ $astc_size -lt $astc_size ]; then echo "FAILED: Lzma better than astc."; fi
done

# Print the global statistics
echo "Global statistics"
echo "Raw: $total_raw_size, Lzma: $total_lzma_size, astc: $total_astc_size"
total_lzma_astc_ratio=$(echo "scale=5; $total_astc_size / $total_lzma_size" | bc)
echo "In general, our compression is smaller than a classic Lzma of $total_lzma_astc_ratio"

# Cleanup the test folders
rm $rawfolder -r
rm $astcfolder -r