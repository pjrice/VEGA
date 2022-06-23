import os
import cv2
import glob
import numpy as np
import pandas as pd

participants = next(os.walk('/home/ausmanpa/gp/VEGA/experiments/E01/data/.'))[1]

participants = [part for part in participants if len(glob.glob('/home/ausmanpa/gp/VEGA/experiments/E01/data/'+part+'/*.jpg'))==34]

partInts = [int(x) for x in participants]

stimFNames = glob.glob('/home/ausmanpa/gp/VEGA/experiments/E01/data/001/*.jpg')

stimFNames = [x[-10:] for x in stimFNames]


for stimulus in stimFNames:
    print('Numbering stimulus '+stimulus+' for all participants...')
    
    if not os.path.isdir('/home/ausmanpa/gp/VEGA/experiments/E01/numberedPartStimImages/'+stimulus[0:-4]):
            os.mkdir('/home/ausmanpa/gp/VEGA/experiments/E01/numberedPartStimImages/'+stimulus[0:-4])
    
    numberedStimImage = cv2.imread('/home/ausmanpa/gp/VEGA/experiments/E01/stimuli/numbered_jpg/'+stimulus)
        
    for part in participants:
        
        partImage = cv2.imread('/home/ausmanpa/gp/VEGA/experiments/E01/data/'+part+'/'+stimulus)
        partImage = np.delete(partImage, (3300), axis=0)
        partImage[np.where(numberedStimImage!=255)] = numberedStimImage[np.where(numberedStimImage!=255)]
            
        cv2.imwrite('/home/ausmanpa/gp/VEGA/experiments/E01/numberedPartStimImages/'+stimulus[0:-4]+'/p'+part+'_'+stimulus[0:-4]+'.jpg',partImage)

