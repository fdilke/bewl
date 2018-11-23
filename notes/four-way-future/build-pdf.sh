#!/usr/bin/env bash

set -e

PANDOC="docker run -v $(pwd):/source jagregory/pandoc"

OPTIONS="\
    -o four-way-future.pdf \
    --latex-engine=xelatex \
    --toc-depth=1"

FILES="\
    title.txt \
    four-way-future.md"

#    scratchpad.md
#    notes.md


$PANDOC $OPTIONS $FILES
