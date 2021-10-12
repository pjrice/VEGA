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
###
### --- History ---
### 2021.09.14   author [0.1]
###             : First commit
##############################################################################

import math
import actr

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

def point_collision(argDict):
    """Calculates the euclidian distance between two points.
       Returns True if the distance is less than the given radius; otherwise,
       False."""
    
    p1 = argDict['point1']
    p2 = argDict['point2']
    radius = argDict['radius']
    useZ = argDict['useZ']
    
    p1x = getattr(p1,'screen-x')
    p1y = getattr(p1,'screen-y')
    p2x = getattr(p2,'screen-x')
    p2y = getattr(p2,'screen-y')
    
    if not useZ:
        ptDist = math.sqrt((p2x-p1x)**2 + (p2y-p1y)**2)
    else:
        try:
            p1z = getattr(p1,'screen-z')
            p2z = getattr(p2,'screen-z')
            ptDist = math.sqrt((p2x-p1x)**2 + (p2y-p1y)**2 + (p2z-p1z)**2)
        except AttributeError:
            raise
            
    return(ptDist<=radius)
    
    
def box_collision():
    pass

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
                
                for target in pts2check:
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
    
    vgScene = visGroups(currentVisicon,25,'point')
    
actr.add_command("pd-mon",proc_display_monitor)
actr.current_connection.interface.send("monitor","proc-display","pd-mon","before")


def test():
    actr.reset()
    
    # just for me to confirm proc-display occurs
    #actr.set_parameter_value(':v',True)
    
    actr.set_parameter_value(':force-visual-commands',True)
    
    
    


    ids = actr.add_visicon_features(['screen-x',10,'screen-y',20,'color','blue'],
                                    ['screen-x',20,'screen-y',30,'color','red'],
                                    ['screen-x',0,'screen-y',0,'color','green'],
                                    ['screen-x',500,'screen-y',500,'color','orange'],
                                    ['screen-x',500,'screen-y',500,'color','purple'],
                                    ['screen-x',500,'screen-y',500,'color','turquoise'])
    
    actr.delete_visicon_features(ids[1])
    actr.run(.1)
    #actr.print_visicon()
    #actr.buffer_chunk("visual-location")
   



