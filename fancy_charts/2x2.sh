#!/bin/bash
#
LAYOUT=2x2
GEO=1050
#OUTFILE=`date +"%Y%m%d%H%M"`
#
while test $# -gt 0; do
  case "$1" in

     -)   shift; LAYOUT="$1";;
    -l*)  LAYOUT="`echo :$1 | sed 's/^:-l//'`";;
     -g)  shift; GEO="$1";;
    -g*)  GEO="`echo :$1 | sed 's/^:-g//'`";;
     -o)  shift; OUTFILE="$1";;
    -o*)  OUTFILE="`echo :$1 | sed 's/^:-o//'`";;

       *)  FF="$FF $1";;
  esac
  shift
done

montage $FF -tile ${LAYOUT} -border 0 -geometry ${GEO} ${OUTFILE}

