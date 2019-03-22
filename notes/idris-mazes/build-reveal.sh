#!/usr/bin/env bash

#
# see https://github.com/jgm/pandoc/wiki/Using-pandoc-to-produce-reveal.js-slides
#

set -e

PANDOC="docker run -v $(pwd):/source jagregory/pandoc"

#OPTIONS="\
#    -o idris-mazes.pdf \
#    --latex-engine=xelatex \
#    --toc-depth=1"

OPTIONS="\
    -t revealjs \
    -s \
    -o idris-mazes.html \
    -V theme=moon \
    -V transition=cube \
    -V revealjs-url=./reveal.js"

FILES="\
    title.txt \
    idris-mazes.md"

$PANDOC $OPTIONS $FILES
