#!/bin/bash
# remove jpgs created during participant packet scan processing

rm data/*.jpg
rm data/*.pdf

for partFolder in $(ls data); do
    rm data/$partFolder/*.jpg
    rm data/$partFolder/stim*.pdf
done
