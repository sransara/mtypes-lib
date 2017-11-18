#! /usr/bin/env sh

sed 's/"/\\"/g;1s/^/let blob = "/;$s/$/"/' template_blob > template.ml
