# global variables

# radius to apply for grouping
#groupingRadius = 8

# collision method to apply during grouping
collisionMethod = 'weighted'

# weights for weighted collision method
collisionWeights = {'proximity':1,
                    'verticalAlignment':1,
                    'horizontalAlignment':1,
                    'leftEdgeAlignment':1,
                    'rightEdgeAlignment':1,
                    'topEdgeAlignment':1,
                    'bottomEdgeAlignment':1,
                    'areaSimilarity':1,
                    'identitySimilarity':1}

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
