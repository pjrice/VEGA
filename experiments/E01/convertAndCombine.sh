#!/bin/bash
# this script will:
# 1. break the participant packet scan out into .jpg images for each separate page;
# 2. remove the first page .jpg, which is an identifier of the participant;
# 3. rename the newly created jpgs with names reflective of the identity of the stimulus, using the stimPacketN_order.txt file that is created alongside the packet itself;
# 3. combine the images of each stimulus using a python script, outputting .jpgs;
# 4. convert the combined .jpgs into .pdfs;
# 5. convert the individual participant jpgs into pdf;
# 6. cat the combined and individual pdfs into a single pdf;
# 7. remove the .jpgs that were created during this process
# the script will only do this for participants that it has not already performed this for

# for each participant directory in the data directory
for partFolder in $(ls data); do
    
    #check to ensure that both the packet scan .pdf and the stimPacketN_order.txt files are in the participant's directory
    #https://stackoverflow.com/questions/6363441/check-if-a-file-exists-with-a-wildcard-in-a-shell-script
    if compgen -G "data/$partFolder/*.pdf" > /dev/null && compgen -G "data/$partFolder/*.txt" > /dev/null; then #add check that output file doesn't exist
        
        # get name of packet scan pdf
        packetPDF=$(ls data/$partFolder/*.pdf)
        
        # convert packet scan pdf pages to jpgs
        echo "Converting pdf pages to jpgs for participant $partFolder..."
        pdftoppm -jpeg -r 300 $packetPDF data/$partFolder/output
        
        # remove the first output page, which is an identifier of the participant
        # (except for part 001 - packet scan doesn't contain this first page)
        if [ $partFolder != "001" ]; then
            rm data/$partFolder/output-01.jpg
        fi
        
        # get filenames of created jpgs into array
        jpgFilenames=($(ls -d data/$partFolder/*.jpg))
        
        # get new jpg filenames from stimPacket order txt file into array
        stimOrderFName=$(ls data/$partFolder/*.txt)
        readarray -t new_jpgFilenames < $stimOrderFName
        
        # rename the jpg files to be reflective of the stimuli
        if [ "${#jpgFilenames[@]}" -eq "${#new_jpgFilenames[@]}" ]; then
            echo "Existing files and new names are of equal length - all good"
            for (( i = 0 ; i < ${#jpgFilenames[@]} ; i++ )); do
                echo "filename was ${jpgFilenames[$i]}, will be "${new_jpgFilenames[$i]:0:6}.jpg""
                mv ${jpgFilenames[$i]} "data/$partFolder/${new_jpgFilenames[$i]:0:6}.jpg"
            done
        else
            echo "WARNING: Existing files and new names are NOT of equal length - files will not be renamed!!!"
        fi
done

# combine the images 
python combineStimImages.py

# convert the combined jpgs into pdf
for combImg in $(ls data/*.jpg); do

    echo "Converting $combImg to pdf..."
    convert $combImg "${combImg:0:11}.pdf"

done

# convert the individual participant jpgs into pdf
for partFolder in $(ls -d data/*/); do
    
    echo "Converting $partFolder images to pdf..."
    for indivImg in $(ls $partFolder*.jpg); do
        convert $indivImg "${indivImg:0:15}.pdf"
    done
    
done

# cat the combined and individual pdfs into a single pdf
for stimulus in $(ls data/*.pdf); do

    # create an empty array, then put the combined stimulus image in the array
    stimPDFs=()
    stimPDFs+=($stimulus)
    
    # now put the individual participant image for each of these stimuli into the array
    for partFolder in $(ls -d data/*/); do
    
        stimPDFs+=($(ls $partFolder${stimulus:5}))
        
    done
    
    #echo ${stimPDFs[@]}
    #echo "combinedStimuli/${stimulus:5}"
    echo "Concatenating combined and individual stimuli..."
    pdftk ${stimPDFs[@]} cat output "combinedStimuli/${stimulus:5}"

done
