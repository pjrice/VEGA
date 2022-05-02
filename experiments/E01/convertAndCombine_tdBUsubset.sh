#!/bin/bash
# convert combined jpgs into pdfs, then cat with the original scans from each participant


partListFName='/home/ausmanpa/gp/VEGA/experiments/E01/data/buParts.txt'
readarray -t partList < $partListFName

# convert the combined jpgs into pdf
for combImg in $(ls data/*.jpg); do

    #echo "${combImg:0:11}.pdf"
    echo "Converting $combImg to pdf..."
    convert $combImg "${combImg:0:11}.pdf"

done

# convert the individual participant jpgs into pdf
for (( i = 0 ; i < ${#partList[@]} ; i++ )); do
    
    echo "Converting data/${partList[$i]} images to pdf..."
    for indivImg in $(ls data/${partList[$i]}/*.jpg); do
        #echo "${indivImg:0:15}.pdf"
        convert $indivImg "${indivImg:0:15}.pdf"
    done
    
done

# cat the combined and individual pdfs into a single pdf

for stimulus in $(ls data/*.pdf); do

    stimPDFs=()
    stimPDFs+=($stimulus)
    
    for (( i = 0 ; i < ${#partList[@]} ; i++ )); do
    
        stimPDFs+=($(ls data/${partList[$i]}/${stimulus:5}))
        
    done
    
    #echo ${stimPDFs[@]}
    #echo "combinedStimuli/${stimulus:5}"
    echo "Concatenating combined and individual stimuli..."
    pdftk ${stimPDFs[@]} cat output "buCombined/${stimulus:5}"

done
