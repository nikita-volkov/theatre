#!/bin/bash
set -eo pipefail

ormolu -ci \
$(find . -name "*.hs" \
  -not -path "./*.stack-work/*" \
  -not -path "./.git/*")

stack build --fast
