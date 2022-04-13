
import os
import cv2
import glob
import numpy as np

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

stimFNames = glob.glob('/home/ausmanpa/gp/VEGA/experiments/E01/data/001/*.jpg')

stimFNames = [x[-10:] for x in stimFNames]


for stimulus in stimFNames:
    print('Combining stimulus '+stimulus+' across participants...')
    imgs = [cv2.imread('/home/ausmanpa/gp/VEGA/experiments/E01/data/'+part+'/'+stimulus) for part in participants]
    combImg = blend(imgs)
    cv2.imwrite('/home/ausmanpa/gp/VEGA/experiments/E01/data/'+stimulus,combImg)
    



