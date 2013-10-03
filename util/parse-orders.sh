#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

die () {
	echo >&2 "$@"
	exit 1
}

[ "$#" -ge 1 ] || die "Usage: $0 orders-dir [processed-orders-dir]"
[ "$#" -le 2 ] || die "Usage: $0 orders-dir [processed-orders-dir]"
[ "$#" -eq 2 ] && mkdir -p "$2"

for i in $1/*.html
do
  w3m -dump $i -cols 100500 | $DIR/parse-order.awk -v FILE=$i
  [ "$#" -eq 2 ] && mv "$i" "$2"
done

