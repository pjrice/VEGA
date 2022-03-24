#!/bin/bash
# Randomize order of stimuli pdfs and cat them together with pdftk

# script requires a single argument: the number of packets to make
numPackets=$1

# get the filenames of existing packets to infer where numbering should start/end
if [ -z "$(ls -A packets)" ]; then 
    #echo "Empty" 
    startVal=1
    endVal=$numPackets
else 
    #echo "Not Empty" 
    existingPackets=(packets/*)

    lastPacketNum=${existingPackets[-1]:18:-4}

    startVal=$((lastPacketNum + 1))
    endVal=$((lastPacketNum + numPackets))
fi


cd pdf

for i in `seq ${startVal} 1 ${endVal}`; do

    echo "Making packet number ${i}..."
    
    # make the packet's filename
    packetName="stimPacket${i}"
    
    # get the list of stimuli filenames
    stimFiles=(*)
    
    # shuffle the list of stimuli filenames
    stimFiles=( $(shuf -e "${stimFiles[@]}") )
    
    # write the shuffled list of filenames to a textfile for recordkeeping
    printf "%s\n" "${stimFiles[@]}" > "../packets/${packetName}_order.txt"
    
    # use pdftk to cat all stimFiles
    pdftk ${stimFiles[@]} cat output "../packets/${packetName}.pdf"
    
    
done