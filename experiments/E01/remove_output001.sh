#!/bin/bash
# remove output-01.jpg if the participant isn't 001

for partFolder in $(ls data); do
    if [ $partFolder != "001" ]; then
        rm data/$partFolder/output-01.jpg
    fi
done
