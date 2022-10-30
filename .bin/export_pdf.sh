#!/bin/bash

filename=$1
template=$2
target="$(dirname $filename)/pdfs"
outputFile="$(basename $filename .md).pdf"

# Make the target directory for pdf file
# mkdir $target

# Academic Paper
if [ "$template" == "-A" ]; then
	echo $template
fi

# Basic export
if [ "$template" == "-B" ]; then
	echo $template
fi

# TODO Create different template exports
