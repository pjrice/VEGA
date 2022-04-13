#!/bin/bash
# convert combined jpgs into pdfs, then cat with the original scans from each participant

# convert the combined jpgs into pdf
for combImg in $(ls data/*.jpg); do

    #echo "${combImg:0:11}.pdf"
    echo "Converting $combImg to pdf..."
    convert $combImg "${combImg:0:11}.pdf"

done

# convert the individual participant jpgs into pdf
for partFolder in $(ls -d data/*/); do
    
    echo "Converting $partFolder images to pdf..."
    for indivImg in $(ls $partFolder*.jpg); do
        #echo "${indivImg:0:15}.pdf"
        convert $indivImg "${indivImg:0:15}.pdf"
    done
    
done

# cat the combined and individual pdfs into a single pdf
for stimulus in $(ls data/*.pdf); do

    stimPDFs=()
    stimPDFs+=($stimulus)
    
    for partFolder in $(ls -d data/*/); do
    
        stimPDFs+=($(ls $partFolder${stimulus:5}))
        
    done
    
    #echo ${stimPDFs[@]}
    #echo "combinedStimuli/${stimulus:5}"
    echo "Concatenating combined and individual stimuli..."
    pdftk ${stimPDFs[@]} cat output "combinedStimuli/${stimulus:5}"

done
