#!/usr/bin/env bash

set -e

PANDOC="docker run -v $(pwd):/source jagregory/pandoc"

OPTIONS="\
    -o scalax-2018-shareback.pdf \
    --latex-engine=xelatex \
    --toc-depth=1"

FILES="\
    title.txt \
    scalax-2018-shareback.md"

#    scratchpad.md
#    notes.md


$PANDOC $OPTIONS $FILES
