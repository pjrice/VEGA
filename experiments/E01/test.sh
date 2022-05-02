#!/bin/bash

partListFName='/home/ausmanpa/gp/VEGA/experiments/E01/data/tdParts.txt'
readarray -t partList < $partListFName

for stimulus in $(ls data/*.pdf); do

    stimPDFs=()
    stimPDFs+=($stimulus)
    
    for (( i = 0 ; i < ${#partList[@]} ; i++ )); do
    
        stimPDFs+=($(ls data/${partList[$i]}/${stimulus:5}))
        
    done
    
    #echo ${stimPDFs[@]}
    #echo "combinedStimuli/${stimulus:5}"
    echo "Concatenating combined and individual stimuli..."
    pdftk ${stimPDFs[@]} cat output "combinedStimuli/${stimulus:5}"

done
