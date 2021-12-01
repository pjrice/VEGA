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
###             : - extend-possible-slots has to be called
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

import math
import actr
import warnings

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
    
    p1x = getattr(p1,'screen-x')
    p1y = getattr(p1,'screen-y')
    p2x = getattr(p2,'screen-x')
    p2y = getattr(p2,'screen-y')
    
    if not useZ:
        ptDist = xy_euclidian_distance((p1x,p1y),(p2x,p2y))
    else:
        try:
            p1z = getattr(p1,'screen-z')
            p2z = getattr(p2,'screen-z')
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
    if hasattr(p1,'width') and hasattr(p1,'height'):
        
        p1x = getattr(p1,'screen-x')
        p1y = getattr(p1,'screen-y')
        p1w = getattr(p1,'width')
        p1h = getattr(p1,'height')
        
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
    
    originX = getattr(originPt,'screen-x')
    originY = getattr(originPt,'screen-y')
    
    if hasattr(targetPt,'width') and hasattr(targetPt,'height'):
        try:
            targetWidth = getattr(targetPt,'width')
            targetHeight = getattr(targetPt,'height')
            targetX = getattr(targetPt,'screen-x')
            targetY = getattr(targetPt,'screen-y')
        
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
# Group labeling utilities

def generic_naming(n):

def determine_naming_type(namingType,n):
    switcher = {
        'generic':generic_naming,
        'sequential':sequential_naming
        }
    
    func = switcher.get(namingType, lambda: "Invalid naming type")
    return(func(n))

def gen_n_syms(n):
    

def label_groups(groupedScene,prevGroupedScene=None):
    
    if prevGroupedScene is None:
        groupedScene.groupNames = gen_n_syms(groupedScene.groupCount)

            
        
        
            


##############################################################################
# ACT-R monitors


# keeping a global list of feature ids and assuming there's only one model for 
# now. Will wrap in actr module later to handle multiple models, etc
features = []
currentVisicon = []

vgScene = None
vgPrevScene = None


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
    
    # Just store the ids in the list
    features = features + results[0]
    
    # insert the feature ID into the feature's details 
    [j.insert(0,i) for i,j in zip(results[0],params)]
    
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
    
    try:
        
        # using the list of current visicon features in currentVisicon, group
        # the scene using the specified radius and collision method
        vgScene = visGroups(currentVisicon,25,'box')
        
        # generate and apply labels for the newly determined groups
        # label application first occurs in the python representation, and then
        # the ACT-R chunk representation of a given feature is modified so that 
        # a "group" slot is added with a value set to the generated group label
        # inherit labels from vgPrevScene if possible
        label_groups(vgScene,vgPrevScene)
        
        
        # display boxes around the visicon content
        
        
        
        # now that groups have been determined and we've done everything we
        # want with them, store the current scene as the previous scene
        vgPrevScene = vgScene
        
        
    except AttributeError:
        raise
    
    
actr.add_command("pd-mon",proc_display_monitor)
actr.current_connection.interface.send("monitor","proc-display","pd-mon","before")


def test():
    actr.reset()
    
    # just for me to confirm proc-display occurs
    #actr.set_parameter_value(':v',True)
    
    actr.set_parameter_value(':force-visual-commands',True)
    
    
    


    ids = actr.add_visicon_features(['screen-x',10,'screen-y',20,'height',1,'width',1,'color','blue'],
                                    ['screen-x',20,'screen-y',30,'height',1,'width',1,'color','red'],
                                    ['screen-x',0,'screen-y',0,'height',1,'width',1,'color','green'],
                                    ['screen-x',510,'screen-y',490,'height',1,'width',1,'color','orange'],
                                    ['screen-x',505,'screen-y',510,'height',1,'width',1,'color','purple'],
                                    ['screen-x',500,'screen-y',515,'height',1,'width',1,'color','turquoise'])
    
    #actr.delete_visicon_features(ids[1])
    actr.run(.1)
    #actr.print_visicon()
    #actr.buffer_chunk("visual-location")
   



