#!/bin/bash

filename=$1
outputFile="./$(basename "$filename" .md).pdf"

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
