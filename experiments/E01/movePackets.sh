#!/bin/bash

for partFolder in $(ls -d data/*/); do
    
    if compgen -G "$partFolder/*.pdf" > /dev/null; then
        pdfName=$(ls $partFolder*.pdf)
        cp $pdfName "/home/ausmanpa/Desktop/partPackets/part_"${pdfName:5:3}".pdf"
        #echo "/home/ausmanpa/Desktop/partPackets/part_"${pdfName:5:3}".pdf"
    fi
done
