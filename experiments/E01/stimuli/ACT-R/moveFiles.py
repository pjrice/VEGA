import os
import glob
import shutil


taskFolder = '/Users/pjr5/Desktop/grouper_3choice/scenes/choice/*/'


xmlFolder = '/Users/pjr5/Desktop/weightedModelPreds/xmls/'


for folder in glob.glob(taskFolder,recursive=False):
    
    originPath = glob.glob(folder+'*.xml')[0]
    
    
    filename = originPath.split('/')[-1]
    
    destPath = xmlFolder+filename
    
    shutil.copyfile(originPath,destPath)
    
    
    