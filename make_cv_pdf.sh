#!/bin/bash

pandoc \
  -N \
  --template=future/mytemplate.tex \
  --variable mainfont=Palatino \
  --variable sansfont=Helvetica \
  --variable monofont=Menlo \
  --variable documentClass=book \
  --variable fontsize=10pt \
  resume.md \
  --toc \
  -o cv.pdf \
  -f markdown
