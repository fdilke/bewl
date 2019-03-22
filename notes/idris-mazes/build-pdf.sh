#!/usr/bin/env bash

set -e

PANDOC="docker run -v $(pwd):/source jagregory/pandoc"

OPTIONS="\
    -o idris-mazes.pdf \
    --latex-engine=xelatex \
    --toc-depth=1"

FILES="\
    title.txt \
    idris-mazes.md"

$PANDOC $OPTIONS $FILES
