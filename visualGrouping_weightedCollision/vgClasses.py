import scipy.stats
import itertools
import matplotlib.pyplot as plt

import vgCollision

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
    def __init__(self, currentVisicon, glomType):
        self.visFeats = currentVisicon
        self.glomRadius = 0
        self.glomType = glomType
        self.visPoints = None
        
        # using the maintained currentVisicon, create a set of visPoint objects
        # self.visPoints will be populated with the result
        if self.visFeats:
            self.build_from_visicon()
        
        self.determine_radius()
        
        # using the info in self.visPoints, perform the grouping here
        if self.glomRadius:
            self.glom_groups(self.glomRadius)
        
    def build_from_visicon(self):
        visPointList = []
        for feat in self.visFeats:
            visPointList.append(visPoint(feat))
            
        self.visPoints = visPointList
        
    def determine_radius(self):
        distances = []
        for pair in itertools.combinations(self.visPoints,2):
            argDict = {'point1':pair[0],
                       'point2':pair[1]}
            distances.append(vgCollision.weighted_collision(argDict))
        
        _, bins, _ = plt.hist(distances)
        mu, sigma = scipy.stats.norm.fit(distances)
        bestFitLine = scipy.stats.norm.pdf(bins,mu,sigma)
        plt.plot(bins,bestFitLine)
        
        # first have to determine why there seemed to be 10 distances rather than 5
        # then, this is a shit approach - 
        # try sorting the list, then finding the largest interval between any two consecutive elements within the list
    
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
                        if vgCollision.determine_pt2pt_collision(self.glomType,argDict):
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
                        
                        if vgCollision.determine_pt2pt_collision(self.glomType,argDict):
                            hits.append(target)
                            
                if len(hits) > 0:
                    self.glom_groups_grow(radius,hits,groupIdx)
                    
    def reset_checked(self):
        for pt in self.visPoints:
            pt.checked = False
