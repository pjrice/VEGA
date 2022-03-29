#!/bin/bash
# Rename .jpg files derived from participant packet scans to corresponding stimulus filenames

# go through each folder in data directory

for partFolder in $(ls data); do
    #https://stackoverflow.com/questions/6363441/check-if-a-file-exists-with-a-wildcard-in-a-shell-script
    if compgen -G "data/$partFolder/*.pdf" > /dev/null && compgen -G "data/$partFolder/*.txt" > /dev/null; then #add check that output file doesn't exist
        
        # get name of packet scan pdf
        packetPDF=$(ls data/$partFolder/*.pdf)
        
        # convert packet scan pdf pages to jpgs
        echo "Converting pdf pages to jpgs for participant $partFolder..."
        #pdftoppm -jpeg -r 300 $packetPDF data/$partFolder/output
        
        # get filenames of created jpgs into array
        jpgFilenames=($(ls -d data/$partFolder/*.jpg))
        #echo ${jpgFilenames[1]}
        # get new jpg filenames from stimPacket order txt file into array
        stimOrderFName=$(ls data/$partFolder/*.txt)
        readarray -t new_jpgFilenames < $stimOrderFName
        #echo ${new_jpgFilenames[1]}
        
        
        # rename the jpg files to be reflective of the stimuli
        #if [ "${#jpgFilenames[@]}" -eq "${#new_jpgFilenames[@]}" ]; then
        #    echo "Existing files and new names are of equal length - all good"
        #    for (( i = 0 ; i < ${#jpgFilenames[@]} ; i++ )); do
        #        #echo "filename was ${jpgFilenames[$i]}, will be "${new_jpgFilenames[$i]:0:6}.jpg""
        #        mv ${jpgFilenames[$i]} "data/$partFolder/${new_jpgFilenames[$i]:0:6}.jpg"
        #    done
        #else
        #    echo "WARNING: Existing files and new names are NOT of equal length - files will not be renamed!!!"
        #fi

    fi
done

# combine the images 
#python combineStimImages.py


