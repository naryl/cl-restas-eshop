#!/bin/bash
[ "$#" -eq 1 ] || die "Usage: $0 source.xls > script.sexp"

xls2csv $1 | tail -n +2 | head -n -1 | awk -F',' '{print "(:key " $1 " :bonuscount " $2 ")"}'
