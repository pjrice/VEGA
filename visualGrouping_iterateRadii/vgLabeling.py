import sys
import platform
import warnings
import numpy as np
if platform.system()=='Linux':
    sys.path.insert(0,'/home/ausmanpa/actr7.x/tutorial/python')
elif platform.system()=='Darwin':
    sys.path.insert(0,'/Users/pjr5/Desktop/actr7.x/tutorial/python')

import actr
import vgConfig
import vgCollision

##############################################################################
# Visicon chunk labeling utilities    

def label_groups(groupedScene,prevGroupedScene=None):
    
    if prevGroupedScene is None:
        groupedScene.groupNames = actr.current_connection.evaluate("gen-n-syms",groupedScene.groupCount)[0]
        for visPoint in groupedScene.visPoints:
            visPoint.groupName = groupedScene.groupNames[visPoint.groupIdx]
    else:
        inherit_group_labels(groupedScene,prevGroupedScene)
    

def inherit_group_labels(groupedScene,prevGroupedScene):
    
    # warn if the collision method used for the two scenes is not the same
    if groupedScene.glomType != prevGroupedScene.glomType:
        warnings.warn("Warning...somehow the collision method used for the previous scene is not the same as the collision method used for the current scene.\nUsing the current scene's collision method for group inheritance.")
      
    # warn if the glomming radius used for the two scenes is not the same
    if groupedScene.glomRadius != prevGroupedScene.glomRadius:
        warnings.warn("Warning...the glomming radius used for the previous scene is not the same as the glomming radius used for the current scene.\nUsing the current scene's glomming radius for group inheritance.")
    
    # initialize collisions table as 2d array 
    # rows index groups of the current scene
    # columns index groups of the previous scene
    collisions = np.zeros(shape=(groupedScene.groupCount,prevGroupedScene.groupCount),dtype=int)
    
    # initialize an empty list to store the new names in 
    newNames = []
    
    # determine whether each of the points between the new and old scenes collide, according to 
    # the new scene's collision method and glomming radius
    # if two points do, increment the collisions element that corresponds to the groups that the 
    # new and old points belong to
    for newPoint in groupedScene.visPoints:
        for oldPoint in prevGroupedScene.visPoints:
            argDict = {'point1':newPoint,
                       'point2':oldPoint,
                       'radius':groupedScene.glomRadius,
                       'useZ':False}
            
            if vgCollision.determine_pt2pt_collision(groupedScene.glomType,argDict):
                collisions[newPoint.groupIdx,oldPoint.groupIdx] += 1
                
    for newGrpIdx in range(0,groupedScene.groupCount):
        # get the number of old groups that contain points that collide with points of the new group
        numNewHits = np.sum([1 if x>0 else 0 for x in collisions[newGrpIdx,:]])
        inheritedName = None
        
        for oldGrpIdx in range(0,prevGroupedScene.groupCount):
            if inheritedName is None:
                # get the number of new groups that contain points that collide with points of the old group
                numOldHits= np.sum([1 if x>0 else 0 for x in collisions[:,oldGrpIdx]])
                numPtHits = collisions[newGrpIdx,oldGrpIdx]
                
                if (numOldHits==1 and numNewHits==1 and numPtHits > 0):
                    inheritedName = prevGroupedScene.groupNames[oldGrpIdx]
                    
        if inheritedName is not None:
            newNames.append(inheritedName)
        else:
            newNames.append(actr.current_connection.evaluate("gen-n-syms",1)[0][0])
            
    # set the inherited and newly determined names as group names for the current scene
    groupedScene.groupNames = newNames
    for visPoint in groupedScene.visPoints:
            visPoint.groupName = groupedScene.groupNames[visPoint.groupIdx]
            
def compute_boundaries(screenX,screenY,height,width):
    
    boundDict = {}
    
    boundDict["SCREEN-LEFT"] = int(((screenX - (width/2))//2)*2)
    boundDict["SCREEN-RIGHT"] = int(((screenX + (width/2))//2)*2)
    boundDict["SCREEN-TOP"] = int(((screenY - (height/2))//2)*2)
    boundDict["SCREEN-BOTTOM"] = int(((screenY + (height/2))//2)*2)

    return boundDict    

# function to modify visicon features from within the modify-visicon-features 
# monitor (called in another thread with threading.Thread)
def call_modify_visicon_features(modVisArgList):
    
    vgConfig.modVisLock = True
    actr.modify_visicon_features(modVisArgList)
    vgConfig.modVisLock = False
                
##############################################################################
# Utility to mark extent of groups on experiment window
        
def denote_group_extent(groupedScenesDict):
    
    # from ACT-R AGI manual
    colorList = ['black','green','dark-gray','cyan','light-blue','yellow',
                 'dark-yellow','light-gray','dark-blue','dark-magenta','white',
                 'dark-cyan','blue','purple','gray','dark-green','dark-red',
                 'red','brown','pink','magenta']
    colorIdx = 0
    
    dictKeys = [key for key in groupedScenesDict.keys()]
    
    for groupedRadius in dictKeys:
        groupedScene = groupedScenesDict[groupedRadius]
    
    
        # groupedScene contains coordinates in visicon space, which are centered on the feature. This function will 
        # draw boxes on the visicon, which uses coordinates that position in the top left. So, will have to convert
        for groupIdx in groupedScene.groupIdxs:
        
            # get the leftmost screen-x coordinate
            groupXcoord = min([getattr(vp,'SCREEN-X')-(getattr(vp,'WIDTH')/2) for vp in groupedScene.visPoints if vp.groupIdx==groupIdx])
        
            # get the topmost screen-y coordinate
            groupYcoord = min([getattr(vp,'SCREEN-Y')-(getattr(vp,'HEIGHT')/2) for vp in groupedScene.visPoints if vp.groupIdx==groupIdx])
        
            # get the rightmost screen-x coordinate, considering width of the visPoints
            groupRXcoord = max([getattr(vp,'SCREEN-X')+(getattr(vp,'WIDTH')/2) for vp in groupedScene.visPoints if vp.groupIdx==groupIdx])
        
            # get the bottommost screen-y coordinate, considering the height of the visPoints
            groupBYcoord = max([getattr(vp,'SCREEN-Y')+(getattr(vp,'HEIGHT')/2) for vp in groupedScene.visPoints if vp.groupIdx==groupIdx])
        
            # draw a box on the experiment window outlining the extent of the group
            actr.add_line_to_exp_window("Ballot",[groupXcoord,groupYcoord],[groupRXcoord,groupYcoord],colorList[colorIdx]) # top edge of box
            actr.add_line_to_exp_window("Ballot",[groupXcoord,groupYcoord],[groupXcoord,groupBYcoord],colorList[colorIdx]) # left edge of box
            actr.add_line_to_exp_window("Ballot",[groupRXcoord,groupYcoord],[groupRXcoord,groupBYcoord],colorList[colorIdx]) # right edge of box
            actr.add_line_to_exp_window("Ballot",[groupXcoord,groupBYcoord],[groupRXcoord,groupBYcoord],colorList[colorIdx]) # bottom edge of box
            
        colorIdx += 1
        if colorIdx > len(colorList):
            colorIdx = 0
