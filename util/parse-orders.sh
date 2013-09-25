#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

for i in $1/*.html
do
  w3m -dump $i -cols 100500 | $DIR/parse-order.awk -v FILE=$i
done

