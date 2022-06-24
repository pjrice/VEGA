import glob



xmlFolder = '/home/ausmanpa/gp/VEGA/experiments/E01/stimuli/numbered_jpg/'
#saveFolder = '/home/ausmanpa/Desktop/testing/'
saveFolder = '/home/ausmanpa/Desktop/testing/iterateRadiiModelPreds/'

xmlFiles = glob.glob(xmlFolder+'*.xml')

for xmlFile in xmlFiles:
    
    # reset ACT-R
    vgLabeling.actr.reset()

    # ensure that "group" is a valid slot name for visicon chunks
    vgLabeling.actr.extend_possible_slots("group", warn=False)
        
    # also have to extend visicon chunk slots with left/right/top/bottom entries
    vgLabeling.actr.extend_possible_slots("screen-left",warn=False)
    vgLabeling.actr.extend_possible_slots("screen-right",warn=False)
    vgLabeling.actr.extend_possible_slots("screen-top",warn=False)
    vgLabeling.actr.extend_possible_slots("screen-bottom",warn=False)
    
    # use the XML to insert the stimulus objects into the visicon
    parseXMLtoACTR(xmlFile)

    # run ACT-R so that the stimulus objects are grouped
    vgLabeling.actr.run(0.1)
    
    # create and save the model prediction jpg
    stimName = xmlFile.split('/')[-1].split('.')[0]
    #mk_modelPred_jpgs(vgConfig.vgScene, stimName, saveFolder)
    mk_hierModelPred_jpgs(vgConfig.sceneGroupsSubset, stimName, saveFolder)
    

    