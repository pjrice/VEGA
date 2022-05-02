
import os
import cv2
import glob
import numpy as np
import pandas as pd

#https://stackoverflow.com/questions/25440865/how-to-blend-multiple-images-in-opencv
def blend(list_images): # Blend images equally.

    equal_fraction = 1.0 / (len(list_images))

    output = np.zeros_like(list_images[0])

    for img in list_images:
        output = output + img * equal_fraction

    output = output.astype(np.uint8)
    return output


participants = next(os.walk('/home/ausmanpa/gp/VEGA/experiments/E01/data/.'))[1]

participants = [part for part in participants if len(glob.glob('/home/ausmanpa/gp/VEGA/experiments/E01/data/'+part+'/*.jpg'))==34]

partInts = [int(x) for x in participants]

stimFNames = glob.glob('/home/ausmanpa/gp/VEGA/experiments/E01/data/001/*.jpg')

stimFNames = [x[-10:] for x in stimFNames]


exptRecord = pd.read_excel('/home/ausmanpa/gp/VEGA/experiments/E01/Recording_sheet.xlsx')
exptRecord.drop(columns=['Start Time', 'End Time','Notes','Packet file name','Experimenter','Date', 'Age','Gender'])


tdParts = exptRecord['Subject #'][exptRecord['Instructions format']=='top-down']
tdParts = list(tdParts)
buParts = exptRecord['Subject #'][exptRecord['Instructions format']=='bottom-up']
buParts = list(buParts)

tdPartList = []
for part in tdParts:
    if part in partInts:
        idx = partInts.index(part)
        tdPartList.append(participants[idx])

buPartList = []
for part in buParts:
    if part in partInts:
        idx = partInts.index(part)
        buPartList.append(participants[idx])


# all participants
for stimulus in stimFNames:
    print('Combining stimulus '+stimulus+' across participants...')
    imgs = [cv2.imread('/home/ausmanpa/gp/VEGA/experiments/E01/data/'+part+'/'+stimulus) for part in participants]
    combImg = blend(imgs)
    cv2.imwrite('/home/ausmanpa/gp/VEGA/experiments/E01/data/'+stimulus,combImg)
    
# top-down participants
for stimulus in stimFNames:
    print('Combining stimulus '+stimulus+' across participants...')
    imgs = [cv2.imread('/home/ausmanpa/gp/VEGA/experiments/E01/data/'+part+'/'+stimulus) for part in tdPartList]
    combImg = blend(imgs)
    cv2.imwrite('/home/ausmanpa/gp/VEGA/experiments/E01/data/'+stimulus,combImg)    

# bottom-up participants
for stimulus in stimFNames:
    print('Combining stimulus '+stimulus+' across participants...')
    imgs = [cv2.imread('/home/ausmanpa/gp/VEGA/experiments/E01/data/'+part+'/'+stimulus) for part in buPartList]
    combImg = blend(imgs)
    cv2.imwrite('/home/ausmanpa/gp/VEGA/experiments/E01/data/'+stimulus,combImg) 

# write TD parts to file
txtfile = open('/home/ausmanpa/gp/VEGA/experiments/E01/data/tdParts.txt','w')
for element in tdPartList:
    txtfile.write(element+'\n')
txtfile.close()


# write BU parts to file
txtfile = open('/home/ausmanpa/gp/VEGA/experiments/E01/data/buParts.txt','w')
for element in buPartList:
    txtfile.write(element+'\n')
txtfile.close()