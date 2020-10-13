#!/bin/bash
# Zamienia plik PNG w układzie MZ na text
F=$1
B=TESTS.csv
##
## usuń niepotrzebne teksty tj. zamaluj na biało:
convert $F -fill white -draw "rectangle 0,0 1200,115" -draw "rectangle 0,640 200,675"  \
 -draw "rectangle 350,115 830,540" test__01.png
##
## usuń wsio za wyjątkiem czarnego:
convert test__01.png -fuzz 35% -fill white +opaque black result__01.png
##
## OCRuj to co wyszło:
tesseract result__01.png result__01 --oem 3 --psm 3
##
## usuń puste wiersze i zamień na jeden, dopisz do bazy wraz z nazwą pliku:
grep '[0-9]' result__01.txt | awk -v DD="$F" 'BEGIN {l=DD}; {gsub(/[ \t]/, ""); l = l ";" $0 }; END{print l}' >> $B
