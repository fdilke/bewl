#!/usr/bin/env bash

set -e

PANDOC="docker run -v $(pwd):/source jagregory/pandoc"

OPTIONS="\
    -o journey-to-fp.pdf \
    --latex-engine=xelatex \
    --toc-depth=1"

FILES="\
    title.txt \
    contents.md \
    journey-to-fp.md"

#    scratchpad.md


$PANDOC $OPTIONS $FILES
