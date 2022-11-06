#!/bin/bash

filename=$1
target="$(dirname $filename)/pdfs"
outputFile="$target/$(basename $filename .md).pdf"

# Make the target directory for pdf file
mkdir $target

pandoc "$filename" \
	--toc \
	-V colorlinks=true\
	-V linkcolor=blue \
	-V toccolor=blue \
	-V geometry:a4paper \
	-V geometry:margin=2cm \
	-V mainfont="DejaVu Serif" \
	-V monofont="DejaVu Serif Mono" \
	-V fontsize=12pt \
	-o "$outputFile"
