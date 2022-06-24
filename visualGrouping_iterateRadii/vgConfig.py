# global variables

# radius to apply for grouping
groupingRadius = 8

# collision method to apply during grouping
collisionMethod = 'box'

# keeping a global list of feature ids and assuming there's only one model for 
# now. Will wrap in actr module later to handle multiple models, etc

features = [] # contains a list of visicon feature IDs
currentVisicon = [] # contains a representation of the current visicon

vgScene = None # the current visual scene, grouped
vgPrevScene = None # the previous visual scene, grouped

# control flags
modVisLock = False # a flag to prevent the modify-visicon-features monitor from firing if it is being called from this script
denoteGroups = False # a flag for whether or not to show the extent of the identified groups on the task window
noImages = True # controls whether or not features that are images are considered during grouping
modelIsVoting = False # controls whether grouping occurs while model is voting

radiusBounds = (8,500)
        
sceneGroups = {}
sceneGroupsSubset = None
