import numpy as np

import vgConfig
import vgCollision
import vgLabeling

##############################################################################
# visual point and group classes

class visPoint:
    def __init__(self, visiconFeature):
        self.visiconID = visiconFeature[0]
        self.checked = False
        self.groupIdx = None
        self.groupName = None
        self.groupGroupingIter = None
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
    def __init__(self, currVisicon, glomRadius, glomType):
        self.visFeats = currVisicon
        self.glomRadius = glomRadius
        self.glomType = glomType
        self.visPoints = []
        self.groupCount = 0
        self.prevGroupCount = 0
        self.groupNames = []
        self.groupIdxs = []
        self.metaGrouped = False
        self.groupGroupingIters = 0
        
        
        # using the maintained currentVisicon, create a set of visPoint objects
        # self.visPoints will be populated with the result
        if self.visFeats:
            self.build_from_visicon()
        
        # using the info in self.visPoints, perform the grouping here
        if self.glomRadius:
            self.glom_groups(self.glomRadius)
            self.label_groups()
            self.group_groups()
        
    def build_from_visicon(self):
        for feat in self.visFeats:
            self.visPoints.append(visPoint(feat))
            self.visPoints[-1].groupGroupingIter = self.groupGroupingIters
            
    def build_from_featureList(self,featList):
        for feat in featList:
            self.visPoints.append(visPoint(feat))
            self.visPoints[-1].groupGroupingIter = self.groupGroupingIters
        
    def glom_groups(self,radius):
        
        for pt in self.visPoints:
            
            if not pt.checked and pt.groupIdx is None:
                self.groupIdxs.append(self.groupCount)
                pt.checked = True
                pt.groupIdx = self.groupCount
                
                hits = []
                
                for target in self.visPoints:
                    if not target.checked and target.groupIdx is None:
                        
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
            if not pt.checked and pt.groupIdx is None:
                pt.checked = True
                pt.groupIdx = groupIdx
                
                hits = []
                
                for target in self.visPoints:
                    if not target.checked and target.groupIdx is None:
                        
                        argDict = {'point1':pt,
                                   'point2':target,
                                   'radius':radius,
                                   'useZ':False}
                        
                        if vgCollision.determine_pt2pt_collision(self.glomType,argDict):
                            hits.append(target)
                            
                if len(hits) > 0:
                    self.glom_groups_grow(radius,hits,groupIdx)
                    
    
    def group_groups(self):
        
        radiusMod = 2
        
        while not self.metaGrouped:
            
            self.groupGroupingIters += 1
        
            self.make_group_features()
                    
            # determine the features that were just added with make_group_features()
            if vgConfig.noImages:
                groupFeats = [feat for feat in vgConfig.noImageVisicon if feat not in self.visFeats]
            else:
                groupFeats = [feat for feat in vgConfig.currentVisicon if feat not in self.visFeats]
        
            # add the group features to the list of visual features
            self.visFeats.extend(groupFeats)
                        
            # convert the group features to visPoint objects and add them to the visPoint list    
            self.build_from_featureList(groupFeats)
                        
            self.prevGroupCount = self.groupCount
                
            self.glom_groups(self.glomRadius*radiusMod)
                        
            self.label_groups()
                        
            radiusMod = radiusMod*2
            
            if (self.groupCount - self.prevGroupCount)==1:
                self.metaGrouped = True
        
        
        
        
    
    def make_group_features(self):
                
        #groupFeatureIds = []
        
        for groupIdx in self.groupIdxs[self.prevGroupCount:]:
            
            groupXs = []
            groupYs = []
            groupSLefts = []
            groupSRights = []
            groupSTops = []
            groupSBottoms = []
            
            for visPoint in self.visPoints:
                if visPoint.groupIdx == groupIdx:
                    groupXs.append(getattr(visPoint,'SCREEN-X'))
                    groupYs.append(getattr(visPoint,'SCREEN-Y'))
                    groupSLefts.append(getattr(visPoint,'SCREEN-LEFT'))
                    groupSRights.append(getattr(visPoint,'SCREEN-RIGHT'))
                    groupSTops.append(getattr(visPoint,'SCREEN-TOP'))
                    groupSBottoms.append(getattr(visPoint,'SCREEN-BOTTOM'))
            
            groupMeanX = int(np.mean(groupXs))
            groupMeanY = int(np.mean(groupYs))
            
            groupLeft = np.min(groupSLefts)
            groupRight = np.max(groupSRights)
            groupTop = np.min(groupSTops)
            groupBottom = np.max(groupSBottoms)
            
            #groupHeight = int(groupTop - groupBottom)
            groupHeight = int(groupBottom - groupTop) # because groupBottom/Top are based off of screen-top/bottom, which are in top left origin coordinates (screen-x/y)
            groupWidth = int(groupRight - groupLeft)
            
            vgLabeling.actr.add_visicon_features(['ISA',['VISUAL-LOCATION', 'GROUP'],
                                        'SCREEN-X',groupMeanX,
                                        'SCREEN-Y',groupMeanY,
                                        'HEIGHT',groupHeight,
                                        'WIDTH',groupWidth,
                                        'VALUE',self.groupNames[groupIdx]])
            
        
    
    def reset_checked(self):
        for pt in self.visPoints:
            pt.checked = False
            
    def label_groups(self):
        self.groupNames.extend(vgLabeling.actr.current_connection.evaluate("gen-n-syms",self.groupCount-self.prevGroupCount)[0])
        
        for pt in self.visPoints:
            if pt.groupName is None:
                pt.groupName = self.groupNames[pt.groupIdx]
                #vgLabeling.actr.set_chunk_slot_value(pt.visiconID,"GROUP",pt.groupName)
                vgConfig.modVisLock = True
                vgLabeling.actr.modify_visicon_features([pt.visiconID,"GROUP",pt.groupName])
                vgConfig.modVisLock = False























