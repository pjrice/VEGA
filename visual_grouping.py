# ----------------------------------------------------------------------
# Begin file: visual-grouping.py
# ----------------------------------------------------------------------


##############################################################################
### 
### Author      : Patrick J. Rice
### Address     : Rice University
###             : Psychology Department
###             : Houston,TX 77251-1892
###             : emailhere
### 
### Copyright   : (c)2021 Patrick J. Rice
### Availability: Covered by the GNU LGPL, see LGPL.txt
### 
##############################################################################
### 
### Filename    : visual-grouping.py
### Version     : 0.1
### 
### Description : A translation of John K. Lindstedt's LISP VEGA model to python,
###             : using the ACT-R RPC interface 
### 
### Bugs        : - 
###             
###
### Todo        : - create specialized actr.py for this application
###                 specifically, adding explicit support for before/after 
###                 monitoring and reducing it down to only the necessary
###                 components
###             : - wrap this in an actr module, as suggested by Dan. He has
###                 some comments on this in his modvis.py example
###             : - implement 3d collision? (ie, 3d euclidian distance)
###             : - the attribute error that occurs if 3d collision is used
###                 without screen-z being defined is silent
###             : - presumably, the same with the attribute error that would be
###                 raised in box_nearest_pt() if targetPoint does not have
###                 width/height attributes
###
### --- History ---
### 2021.09.14   author [0.1]
###             : First commit
##############################################################################

import sys
import time
import math
import warnings
import numpy as np

sys.path.insert(0,'/home/ausmanpa/actr7.x/tutorial/python')
#sys.path.insert(0,'/Users/pjr5/Desktop/actr7.x/tutorial/python')

import actr

##############################################################################
# global variables

# keeping a global list of feature ids and assuming there's only one model for 
# now. Will wrap in actr module later to handle multiple models, etc

features = [] # contains a list of visicon feature IDs
currentVisicon = [] # contains a representation of the current visicon

vgScene = None # the current visual scene, grouped
vgPrevScene = None # the previous visual scene, grouped

modVisLock = False # a flag to prevent the modify-visicon-features monitor from firing if it is being called from this script

denoteGroups = True # a flag for whether or not to show the extent of the identified groups on the task window

##############################################################################
# collision methods
#   - collision methods should accept one argument only - a dictionary of 
#     parameter:value arguments for the given collision method, which must be
#     parsed by the method itself
#   - the arg dictionary contains at least two key:value pairs:
#       - 'point1': the first visPoint object eligible for collision testing
#       - 'point2': the second visPoint object eligible for collision testing
#   - collision methods must return only T/F - whether or not the two points
#     'collide'

def xy_euclidian_distance(p1,p2):
    """Calculates the euclidian distance between the x/y position of two 
       points. p1 and p2 should be input as (x,y) positions."""
    
    p1x = p1[0]
    p1y = p1[1]
    p2x = p2[0]
    p2y = p2[1]
    
    dist = math.sqrt((p2x-p1x)**2 + (p2y-p1y)**2)
    
    return(dist)

def point_collision(argDict):
    """Determines if the euclidian distance between two points is less than a 
       given radius. Returns True if the distance is less than or equal to the
       given radius; otherwise, False."""
    
    p1 = argDict['point1']
    p2 = argDict['point2']
    radius = argDict['radius']
    useZ = argDict['useZ']
    
    p1x = getattr(p1,'SCREEN-X')
    p1y = getattr(p1,'SCREEN-Y')
    p2x = getattr(p2,'SCREEN-X')
    p2y = getattr(p2,'SCREEN-Y')
    
    if not useZ:
        ptDist = xy_euclidian_distance((p1x,p1y),(p2x,p2y))
    else:
        try:
            p1z = getattr(p1,'SCREEN-Z')
            p2z = getattr(p2,'SCREEN-Z')
            ptDist = math.sqrt((p2x-p1x)**2 + (p2y-p1y)**2 + (p2z-p1z)**2)
        except AttributeError:
            raise
            
    return(ptDist<=radius)
    
    
def box_collision(argDict):
    
    p1 = argDict['point1']
    p2 = argDict['point2']
    radius = argDict['radius']
    
    # box_nearest_pt creates a box around p2 using p2's x/y and width/height
    # information, and then determines the point along this box boundary that
    # is closest to the x/y location of p1 (returns False if p1 is within 
    # p2's box)
    target = box_nearest_pt(p1,p2)
    
    # defines a box around p1, and determines whether 
    if hasattr(p1,'WIDTH') and hasattr(p1,'HEIGHT'):
        
        p1x = getattr(p1,'SCREEN-X')
        p1y = getattr(p1,'SCREEN-Y')
        p1w = getattr(p1,'WIDTH')
        p1h = getattr(p1,'HEIGHT')
        
        leftEdge = p1x - (p1w/2)
        rightEdge = p1x + (p1w/2)
        bottomEdge = p1y - (p1h/2)
        topEdge = p1y + (p1h/2)
    else:
        # these were set to 0 in John's original code but this isn't sensible
        leftEdge = 1
        rightEdge = 1
        bottomEdge = 1
        topEdge = 1
        
    # in John's words: "if there was a target, check it, otherwise we are overlapping and just return T"
    if target:
        
        tx = target[0]
        ty = target[1]
        
        # if the target point's x position is within the left and right edges of point1's box, and the target point's y position is within the top/bottom edges padded by the given radius,
        # it's a hit vertically. hitHorz is the same vice-versa: within the top/bottom edge, and within the left/right edges padded by the given radius
        hitVert = (tx >= leftEdge) and (tx <= rightEdge) and (ty <= (topEdge+radius)) and (ty >= (bottomEdge-radius))
        hitHorz = (tx >= (leftEdge-radius)) and (tx <= (rightEdge+radius)) and (ty <= topEdge) and (ty >= bottomEdge)
        
        # for top/bottom left/right hits, draws a circle of radius equal to the given radius around each corner of p1's box, hit if the target falls within the circle?
        hitTopLeft = (xy_euclidian_distance((leftEdge,topEdge),(tx,ty)) <= radius)
        hitTopRight = (xy_euclidian_distance((rightEdge,topEdge),(tx,ty)) <= radius)
        
        hitBottomLeft = (xy_euclidian_distance((leftEdge,bottomEdge),(tx,ty)) <= radius)
        hitBottomRight = (xy_euclidian_distance((rightEdge,bottomEdge),(tx,ty)) <= radius)
        
        ptsCollide = (hitVert or hitHorz or hitTopLeft or hitTopRight or hitBottomLeft or hitBottomRight)
        
    else:
        ptsCollide = True
    
    
    return(ptsCollide)
    

def box_nearest_pt(originPt,targetPt):
    """Helper function for box_collision(). If targetPt has a height/width,
        returns the point on the perimeter of the box around targetPt (defined
        by the height/width) that is closest to the x/y position of the 
        originPt. If targetPt does not have a height/width, assume a 
        height/width of 1. If originPt falls within the box defined by the 
        targetPt's height/width, returns False."""
    
    originX = getattr(originPt,'SCREEN-X')
    originY = getattr(originPt,'SCREEN-Y')
    
    if hasattr(targetPt,'WIDTH') and hasattr(targetPt,'HEIGHT'):
        try:
            targetWidth = getattr(targetPt,'WIDTH')
            targetHeight = getattr(targetPt,'HEIGHT')
            targetX = getattr(targetPt,'SCREEN-X')
            targetY = getattr(targetPt,'SCREEN-Y')
        
            leftEdge = targetX - (targetWidth/2)
            rightEdge = targetX + (targetWidth/2)
            bottomEdge = targetY - (targetHeight/2)
            topEdge = targetY + (targetHeight/2)
        except AttributeError:
            raise
    else:
        # these were set to 0 in John's original code
        warnings.warn("Warning...assuming visual feature has width/height of 1, as they are not defined as attributes of the feature")
        leftEdge = 1
        rightEdge = 1
        bottomEdge = 1
        topEdge = 1
        
    # first four if statements check if origin point is outside of left/right
    # edges AND top/bottom edges;
    # second four if statements check if origin point is OUTSIDE of left/right
    # edge and WITHIN the top/bottom edges, or vice-versa
    if (originX < leftEdge) and (originY < bottomEdge):
        closestPt = (leftEdge,bottomEdge)
    elif (originX < leftEdge) and (originY > topEdge):
        closestPt = (leftEdge,topEdge)
    elif (originX > rightEdge) and (originY < bottomEdge):
        closestPt = (rightEdge,bottomEdge)
    elif (originX > rightEdge) and (originY > topEdge):
        closestPt = (rightEdge,topEdge)
    elif (originX < leftEdge) and (originY >= bottomEdge) and (originY <= topEdge):
        closestPt = (leftEdge, originY)
    elif (originX > rightEdge) and (originY >= bottomEdge) and (originY <= topEdge):
        closestPt = (rightEdge,originY)
    elif (originY > topEdge) and (originX >= leftEdge) and (originX <= rightEdge):
        closestPt = (originX,topEdge)
    elif (originY < bottomEdge) and (originX >= leftEdge) and (originX <= rightEdge):
        closestPt = (originX,bottomEdge)
    else:
        closestPt = False
        
    return(closestPt)
        
            

def determine_pt2pt_collision(mode,argDict):
    switcher = {
        'point':point_collision,
        'box':box_collision
        }
    
    func = switcher.get(mode, lambda: "Invalid collision method")
    return(func(argDict))
##############################################################################
# visual point and group classes

class visPoint:
    def __init__(self, visiconFeature):
        self.visiconID = visiconFeature[0]
        self.checked = False
        self.groupIdx = None
        self.groupName = None
        self.parse_feature(visiconFeature)
        
    def parse_feature(self,attrList):
        
        # the first entry in the attribute list is the visicon ID of the
        # feature; unnecessary here
        attrOnly = attrList[1:]
        
        # attrOnly should be of length that is evenly divisible by 2 (some 
        # number of paired arg/values)
        if (len(attrOnly) % 2) != 0:
            raise Exception("Length of feature attribute list is incorrect!")
            
        # the attributes of the feature (attrOnly) are established as 
        # slot:value pairs; pair the elements of the attrOnly list
        # pythonic and efficient method of doing pairing:
        # https://stackoverflow.com/questions/4628290/pairs-from-single-list
        attrPairs = zip(attrOnly[::2], attrOnly[1::2])
        
        # using the established pairs, define the attributes of the feature as
        # variables of the visPoint class
        # because it is unknown head of time what the attributes of a given
        # feature will be named, use setattr() to utilize the string names
        # given by the add-visicon-features call
        # additionally, could replace any "-" characters with "_", otherwise 
        # will have to use getattr to address the class variable
        for pair in attrPairs:
            setattr(self, pair[0], pair[1])
            #setattr(self, pair[0].replace("-","_"), pair[1])
             

class visGroups:
    def __init__(self, currentVisicon, glomRadius, glomType):
        self.visFeats = currentVisicon
        self.glomRadius = glomRadius
        self.glomType = glomType
        self.visPoints = None
        
        # using the maintained currentVisicon, create a set of visPoint objects
        # self.visPoints will be populated with the result
        if self.visFeats:
            self.build_from_visicon()
        
        # using the info in self.visPoints, perform the grouping here
        if self.glomRadius:
            self.glom_groups(self.glomRadius)
        
    def build_from_visicon(self):
        visPointList = []
        for feat in self.visFeats:
            visPointList.append(visPoint(feat))
            
        self.visPoints = visPointList
        
    def glom_groups(self,radius):
        
        self.groupCount = 0
        self.groupNames = []
        self.groupIdxs = []
        
        for pt in self.visPoints:
            
            if not pt.checked:
                self.groupIdxs.append(self.groupCount)
                pt.checked = True
                pt.groupIdx = self.groupCount
                
                hits = []
                
                for target in self.visPoints:
                    if not target.checked:
                        
                        argDict = {'point1':pt,
                                   'point2':target,
                                   'radius':radius,
                                   'useZ':False}
                        # if the points collide (collide method that returns T/F), add it to the list of hits
                        if determine_pt2pt_collision(self.glomType,argDict):
                            hits.append(target)
                            
                if len(hits) > 0:
                    self.glom_groups_grow(radius,hits,self.groupCount)
                    
                self.groupCount += 1
                
        self.reset_checked()
                
    def glom_groups_grow(self,radius,pts2check,groupIdx):
        
        for pt in pts2check:
            if not pt.checked:
                pt.checked = True
                pt.groupIdx = groupIdx
                
                hits = []
                
                for target in self.visPoints:
                    if not target.checked:
                        
                        argDict = {'point1':pt,
                                   'point2':target,
                                   'radius':radius,
                                   'useZ':False}
                        
                        if determine_pt2pt_collision(self.glomType,argDict):
                            hits.append(target)
                            
                if len(hits) > 0:
                    self.glom_groups_grow(radius,hits,groupIdx)
                    
    def reset_checked(self):
        for pt in self.visPoints:
            pt.checked = False
        
                        
                        
                
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
            
            if determine_pt2pt_collision(groupedScene.glomType,argDict):
                collisions[newPoint.groupIdx,oldPoint.groupIdx] += 1
                
    for newGrpIdx in range(0,groupedScene.groupCount):
        numNewHits = np.sum(collisions[newGrpIdx,:])
        inheritedName = None
        
        for oldGrpIdx in range(0,prevGroupedScene.groupCount):
            if inheritedName is None:
                numOldHits = np.sum(collisions[:,oldGrpIdx])
                numPtHits = collisions[newGrpIdx,oldGrpIdx]
                
                if (numOldHits==1 and numNewHits==1 and numPtHits > 0):
                    inheritedName = prevGroupedScene.groupNames[oldGroupIdx]
                    
        if inheritedName is not None:
            newNames.append(inheritedName)
        else:
            newNames.append(actr.current_connection.evaluate("gen-n-syms",1)[0][0])
            
    # set the inherited and newly determined names as group names for the current scene
    groupedScene.groupNames = newNames
    for visPoint in groupedScene.visPoints:
            visPoint.groupName = groupedScene.groupNames[visPoint.groupIdx]
            
def compute_boundary(visiconPoint,boundary):
    
    if boundary == "left":
        pointWidth = getattr(visiconPoint,"WIDTH")
        pointX = getattr(visiconPoint,"SCREEN-X")
        boundVal = ((pointX - (pointWidth/2))//2)*2
    elif boundary == "right":
        pointWidth = getattr(visiconPoint,"WIDTH")
        pointX = getattr(visiconPoint,"SCREEN-X")
        boundVal = ((pointX + (pointWidth/2))//2)*2
    elif boundary == "top":
        pointHeight = getattr(visiconPoint,"HEIGHT")
        pointY = getattr(visiconPoint,"SCREEN-Y")
        boundVal = ((pointY - (pointHeight/2))//2)*2 #should probably be +
    elif boundary == "bottom":
        pointHeight = getattr(visiconPoint,"HEIGHT")
        pointY = getattr(visiconPoint,"SCREEN-Y")
        boundVal = ((pointY + (pointHeight/2))//2)*2 #should probably be -
        
    return boundVal

def compute_boundaries(screenX,screenY,height,width):
    
    boundDict = {}
    
    boundDict["SCREEN-LEFT"] = int(((screenX - (width/2))//2)*2)
    boundDict["SCREEN-RIGHT"] = int(((screenX + (width/2))//2)*2)
    boundDict["SCREEN-TOP"] = int(((screenY - (height/2))//2)*2) #should probably be +
    boundDict["SCREEN-BOTTOM"] = int(((screenY + (height/2))//2)*2) #should probably be -

    return boundDict    

##############################################################################
# Utility to mark extent of groups on experiment window
        
def denote_group_extent(groupedScene):
    
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
        actr.add_line_to_exp_window("Ballot",[groupXcoord,groupYcoord],[groupRXcoord,groupYcoord]) # top edge of box
        actr.add_line_to_exp_window("Ballot",[groupXcoord,groupYcoord],[groupXcoord,groupBYcoord]) # left edge of box
        actr.add_line_to_exp_window("Ballot",[groupRXcoord,groupYcoord],[groupRXcoord,groupBYcoord]) # right edge of box
        actr.add_line_to_exp_window("Ballot",[groupXcoord,groupBYcoord],[groupRXcoord,groupBYcoord]) # bottom edge of box
        
        
##############################################################################
# ACT-R interfacing

# ensure that "group" is a valid slot name for visicon chunks
actr.extend_possible_slots("group", warn=False)
        
# also have to extend visicon chunk slots with left/right/top/bottom entries
actr.extend_possible_slots("screen-left",warn=False)
actr.extend_possible_slots("screen-right",warn=False)
actr.extend_possible_slots("screen-top",warn=False)
actr.extend_possible_slots("screen-bottom",warn=False)



# If the model is reset, clear the list of features
def reset ():
    global features
    global currentVisicon
    global vgScene
    global vgPrevScene

    features = []
    currentVisicon = []
    
   
# add this function as an actr command, so that it can be monitored
actr.add_command("modvis-reset",reset)

# monitor the actr reset-start signal, which is generated by the reset command
# unlike clear-all, reset clears the event cue and restores the current set of
# models and modules to their initial states (clear-all removes all models and
# modules)
# a monitor is used to have a command evaluated automatically when another
# command is evaluated - so, the "mod-vis" reset command (which calls the 
# reset() function defined above) is evaluated when the reset-start signal
# is generated
# When this is converted to an actr module, instead of monitoring reset-start, 
# it should be called as the reset function for the module
actr.monitor_command("reset-start","modvis-reset")


# This function will be monitoring the add-visicon-features
# command as an after monitor which means it will be passed
# four parameters: 
#  the name of the command that was called
#  the list of parameters passed to that command
#  a boolean that indicates whether the call completed successfully
#  a list of the return values from the command if it was successful
# Because :force-visual-commands is enabled, all calls to add-visual-features
# (from the remote interfaces as well as internally to actr) will use the actr
# "command" version (always the case for the remote interface, not always for 
# internal actr, when parameter is not enabled) so that it can be monitored.
# Otherwise, some internal actr calls would use the underlying function, and
# not be caught by the monitor
def features_added(cmd,params,success,results):
    # 'results' parameter contains the visicon feature IDs returned by add-visicon-features
    # 'params' parameter contains the parameters used by the add-visicon-features call
    
    global features
    global currentVisicon
    global vgScene
    global vgPrevScene
    global modVisLock
    
    screenXVal = params[0][params[0].index('SCREEN-X') + 1]
    screenYVal = params[0][params[0].index('SCREEN-Y') + 1]
    
    # height/width may not be defined in the (add-visicon-features) call
    if ('HEIGHT' in params[0]) and ('WIDTH' in params[0]):
        heightVal = params[0][params[0].index('HEIGHT') + 1]
        widthVal = params[0][params[0].index('WIDTH') + 1]
    else:
        heightVal = 1
        widthVal = 1
    
    #compute the left/right/top/bottom boundaries that are used by the models
    boundaries = compute_boundaries(screenXVal,screenYVal,heightVal,widthVal)
    
    #modify the visicon entry for this feature with the computed boundaries
    modVisLock = True
    actr.modify_visicon_features([results[0][0],"screen-left",boundaries["SCREEN-LEFT"],
                                             "screen-right",boundaries["SCREEN-RIGHT"],
                                             "screen-top",boundaries["SCREEN-TOP"],
                                             "screen-bottom",boundaries["SCREEN-BOTTOM"]])
    modVisLock = False
    
    # Just store the ids in the list
    features = features + results[0]
    
    # insert the feature ID into the feature's details 
    [j.insert(0,i) for i,j in zip(results[0],params)]
    
    # update the params list with the calculated boundaries
    boundsList = [val for pair in zip(boundaries.keys(),boundaries.values()) for val in pair]
    params[0] = params[0]+boundsList
    
    currentVisicon = currentVisicon + params
    
    
    

# add this function as an actr command, so that it can be monitored    
actr.add_command("modvis-add",features_added)

# actr.py doesn't support after and before monitoring since it's not
# used in the tutorial.  So this depends on the internals of that
# code to send the appropriate message which makes it unsafe for
# compatibility.
# so, will implement actr.monitor_command_before and actr.monitor_command_after
# methods
actr.current_connection.interface.send("monitor","add-visicon-features","modvis-add","after")

# Monitor for the modification of features
def features_modified(cmd,params,success,results):
    global currentVisicon
    global modVisLock
    
    if modVisLock:
        pass
    else:
        # find the indices of the modified features in the currentVisicon
        modFeatIdxs = []
        for f in results[0]:
            modFeatIdxs.append(next(i for i,v in enumerate(currentVisicon) if f in v))
            
        print(modFeatIdxs)
        
        # update currentVisicon to reflect the modifications to the modified features
        for i in range(len(modFeatIdxs)):
            
            # currentVisicon index of the feature we're modifying
            cvIdx = modFeatIdxs[i]
            
            # list of the attributes that were modified for this feature - this includes added and deleted attributes
            moddedAttrs = params[i][1::2]
            print(moddedAttrs)
            
            # list of the new values of the modified attributes - this includes added and deleted attribute values
            newAttrVals = params[i][2::2]
            print(newAttrVals)
            
            # go through the currentVisicon feature representation and determine which were changed, added, deleted
            
            
            
        
        # determine new attributes, changed attributes, and deleted attributes
        #for p in params:
        #    print(p)
            
        
        
    

actr.add_command("modvis-mod",features_modified)
actr.current_connection.interface.send("monitor","modify-visicon-features","modvis-mod","after")

# Similar monitors for the removal of features
def features_removed(cmd,params,success,results):
    # 'results' parameter contains the visicon feature IDs returned by delete-visicon-features
    global features
    global currentVisicon
    global vgScene
    global vgPrevScene
    

    for f in results[0]:
        features.remove(f)
        
        featIdx = [x for x in range(len(currentVisicon)) if currentVisicon[x][0]==f]
        del currentVisicon[featIdx[0]]
        

actr.add_command("modvis-remove",features_removed)
actr.current_connection.interface.send("monitor","delete-visicon-features","modvis-remove","after")


def all_features_removed():
    global features
    global currentVisicon
    global vgScene
    global vgPrevScene
    
    features = []
    currentVisicon = []

actr.add_command("modvis-remove-all",all_features_removed)
actr.monitor_command("delete-all-visicon-features","modvis-remove-all")


def proc_display_monitor(cmd,params,success,results):
    global features
    global currentVisicon
    global vgScene
    global vgPrevScene
    global modVisLock
    
    try:
        
        # using the list of current visicon features in currentVisicon, group
        # the scene using the specified radius and collision method
        #radius= 8 for No-Lines-Color-Box
        vgScene = visGroups(currentVisicon,25,'box')
        
        # generate and apply labels for the newly determined groups
        # label application first occurs in the python representation, and then
        # the ACT-R chunk representation of a given feature is modified so that 
        # a "group" slot is added with a value set to the generated group label
        # inherit labels from vgPrevScene if possible
        label_groups(vgScene,vgPrevScene)
        
        # add the group label associated with each feature in the visicon to
        # feature's chunk representation by adding a slot named "group" with
        # a value set to the label
        modVisLock = True
        for visPoint in vgScene.visPoints:
            #actr.set_chunk_slot_value(visPoint.visiconID,"group",visPoint.groupIdx)
            #actr.set_chunk_slot_value(visPoint.visiconID,"group",visPoint.groupName)
            actr.modify_visicon_features([visPoint.visiconID,"group",visPoint.groupName])
        modVisLock = False
            

        # display boxes around the visicon content
        if denoteGroups:
            denote_group_extent(vgScene)
        
        
        
        # now that groups have been determined and we've done everything we
        # want with them, store the current scene as the previous scene
        #vgPrevScene = vgScene
        
        
    except AttributeError:
        raise
    
    
actr.add_command("pd-mon",proc_display_monitor)
actr.current_connection.interface.send("monitor","proc-display","pd-mon","before")

##############################################################################

# keeps the script running while the model is running. (clear-all) needs to be called to stop this
#while actr.current_model():
#    time.sleep(0.025)


##############################################################################


def test():
    actr.reset()
    
    # just for me to confirm proc-display occurs
    #actr.set_parameter_value(':v',True)
    
    actr.set_parameter_value(':force-visual-commands',True)
    
    
    


    ids = actr.add_visicon_features(['SCREEN-X',10,'SCREEN-Y',20,'HEIGHT',1,'WIDTH',1,'COLOR','blue'],
                                    ['SCREEN-X',20,'SCREEN-Y',30,'HEIGHT',1,'WIDTH',1,'COLOR','red'],
                                    ['SCREEN-X',0,'SCREEN-Y',0,'HEIGHT',1,'WIDTH',1,'COLOR','green'],
                                    ['SCREEN-X',510,'SCREEN-Y',490,'HEIGHT',1,'WIDTH',1,'COLOR','orange'],
                                    ['SCREEN-X',505,'SCREEN-Y',510,'HEIGHT',1,'WIDTH',1,'COLOR','purple'],
                                    ['SCREEN-X',500,'SCREEN-Y',515,'HEIGHT',1,'WIDTH',1,'COLOR','turquoise'])
    
    
    
    # test changing/deleting/adding attributes w/ (modify-visicon-features) from ACT-R environment to see how monitor handles it
    actr.modify_visicon_features([features[0],"SCREEN-X",100], #just change an attribute
                                 [features[2],"SCREEN-X",100, 'WIDTH',None], #change an attribute and delete an attribute
                                 [features[4],"SCREEN-X",100, "GROUP",'test']) #change an attribute and add an attribute

    
    #actr.delete_visicon_features(ids[1])
    actr.run(.1)
    #actr.print_visicon()
    #actr.buffer_chunk("visual-location")
   

