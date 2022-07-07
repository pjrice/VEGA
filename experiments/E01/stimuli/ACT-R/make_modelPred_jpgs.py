import os
import cv2
import numpy as np

# take the grouped scene of an E01 stimulus and produce a jpg with the groupings indicated
def mk_modelPred_jpgs(scene, stimName, saveFolder, lineWidth=5, groupPad=12, scaleFactor=3):

    # lineWidth is +/- the line center (so, double the argument)
    # groupPad translates a given edge closer to the corresponding page edge
    # scaleFactor should probably always be 3, as the participant jpgs are scaled by 300%
    
    # create the empty matrix that will be filled then used to create the jpg
    # want to create an image equivalently sized to the participant response jpgs, which are shaped as (3300,2550,3)
    # dtype=np.uint8 because that's what the participant response jpgs are read in as
    img2write = np.full((3300,2550,3),255,dtype=np.uint8)
    
    # first, put the stimulus objects into the image
    for vp in scene.visPoints:
        # scene contains coordinates in visicon space, which are centered on the feature
        xCoord = getattr(vp,'SCREEN-X')
        yCoord = getattr(vp,'SCREEN-Y')
        height = getattr(vp,'HEIGHT')
        width = getattr(vp,'WIDTH')
        
        # there is a scaling factor - should be 3?
        xCoord = xCoord*scaleFactor
        yCoord = yCoord*scaleFactor
        height = height*scaleFactor
        width = width*scaleFactor
        
        # compute edges - x/y coords are centered on object
        leftEdge = int(xCoord-(width/2))
        rightEdge = int(xCoord+(width/2))
        topEdge = int(yCoord-(height/2))
        bottomEdge = int(yCoord+(height/2))
    
    
        # "draw" left edge
        img2write[topEdge:bottomEdge,(leftEdge-lineWidth):(leftEdge+lineWidth),:] = np.uint8(0)
        # "draw" right edge
        img2write[topEdge:bottomEdge,(rightEdge-lineWidth):(rightEdge+lineWidth),:] = np.uint8(0)
        # "draw" top edge
        img2write[(topEdge-lineWidth):(topEdge+lineWidth),leftEdge:rightEdge,:] = np.uint8(0)
        # "draw" bottom edge
        img2write[(bottomEdge-lineWidth):(bottomEdge+lineWidth),leftEdge:rightEdge,:] = np.uint8(0)
        
    # second, put the group labelings into the image
    for groupIdx in scene.groupIdxs:
    
        # compute edges - x/y coords are centered on object
        groupLE = int(min([(getattr(vp,'SCREEN-X')-(getattr(vp,'WIDTH')/2)) for vp in scene.visPoints if vp.groupIdx==groupIdx]))
        groupRE = int(max([(getattr(vp,'SCREEN-X')+(getattr(vp,'WIDTH')/2)) for vp in scene.visPoints if vp.groupIdx==groupIdx]))
        groupTE = int(min([(getattr(vp,'SCREEN-Y')-(getattr(vp,'HEIGHT')/2)) for vp in scene.visPoints if vp.groupIdx==groupIdx]))
        groupBE = int(max([(getattr(vp,'SCREEN-Y')+(getattr(vp,'HEIGHT')/2)) for vp in scene.visPoints if vp.groupIdx==groupIdx]))
        
        # apply padding and scaling factors
        groupLE = (groupLE-groupPad)*3
        groupRE = (groupRE+groupPad)*3
        groupTE = (groupTE-groupPad)*3
        groupBE = (groupBE+groupPad)*3
    
        # "draw" left edge
        img2write[groupTE:groupBE,(groupLE-lineWidth):(groupLE+lineWidth),:] = [np.uint8(0),np.uint8(0),np.uint8(255)]
        # "draw" right edge
        img2write[groupTE:groupBE,(groupRE-lineWidth):(groupRE+lineWidth),:] = [np.uint8(0),np.uint8(0),np.uint8(255)]
        # "draw" top edge
        img2write[(groupTE-lineWidth):(groupTE+lineWidth),groupLE:groupRE,:] = [np.uint8(0),np.uint8(0),np.uint8(255)]
        # "draw" bottom edge
        img2write[(groupBE-lineWidth):(groupBE+lineWidth),groupLE:groupRE,:] = [np.uint8(0),np.uint8(0),np.uint8(255)]
        
    #prep folder/filename
    if not os.path.isdir(saveFolder+str(scene.glomRadius)+'px/'):
        saveFolder = saveFolder+str(scene.glomRadius)+'px/'
        os.mkdir(saveFolder)
    else:
        saveFolder = saveFolder+str(scene.glomRadius)+'px/'
    
    #write image
    cv2.imwrite(saveFolder+stimName+'.jpg',img2write)



def mk_iterModelPred_jpgs(scene, stimName, saveFolder, lineWidth=5, groupPad=12, scaleFactor=3):
    
    img2write = np.full((3300,2550,3),255,dtype=np.uint8)
    
    colorList = ['red','green','blue','pink','orange','cyan','purple']
    colors = {'red':[np.uint8(0),np.uint8(0),np.uint8(255)],
              'green':[np.uint8(0),np.uint8(255),np.uint8(0)],
              'blue':[np.uint8(255),np.uint8(0),np.uint8(0)],
              'pink':[np.uint8(255),np.uint8(0),np.uint8(255)],
              'orange':[np.uint8(0),np.uint8(165),np.uint8(255)],
              'cyan':[np.uint8(255),np.uint8(255),np.uint8(0)],
              'purple':[np.uint8(128),np.uint8(0),np.uint8(128)]}
    
    radii = list(scene.keys())
    radii.reverse()
    
    # first, put the stimulus objects into the image - only need to do this once
    for vp in scene[radii[0]].visPoints:
        # scene contains coordinates in visicon space, which are centered on the feature
        xCoord = getattr(vp,'SCREEN-X')
        yCoord = getattr(vp,'SCREEN-Y')
        height = getattr(vp,'HEIGHT')
        width = getattr(vp,'WIDTH')
        
        # there is a scaling factor - should be 3?
        xCoord = xCoord*scaleFactor
        yCoord = yCoord*scaleFactor
        height = height*scaleFactor
        width = width*scaleFactor
    
        # compute edges - x/y coords are centered on object
        leftEdge = int(xCoord-(width/2))
        rightEdge = int(xCoord+(width/2))
        topEdge = int(yCoord-(height/2))
        bottomEdge = int(yCoord+(height/2))
    
        # "draw" left edge
        img2write[topEdge:bottomEdge,(leftEdge-lineWidth):(leftEdge+lineWidth),:] = np.uint8(0)
        # "draw" right edge
        img2write[topEdge:bottomEdge,(rightEdge-lineWidth):(rightEdge+lineWidth),:] = np.uint8(0)
        # "draw" top edge
        img2write[(topEdge-lineWidth):(topEdge+lineWidth),leftEdge:rightEdge,:] = np.uint8(0)
        # "draw" bottom edge
        img2write[(bottomEdge-lineWidth):(bottomEdge+lineWidth),leftEdge:rightEdge,:] = np.uint8(0)
    
    # second, put the group labelings into the image
    colorIdx = 0
    for radius in radii:
        
        sceneSubset = scene[radius]
        
        for groupIdx in sceneSubset.groupIdxs:
    
            # compute edges as though x/y coords are top left corner of object
            groupLE = int(min([(getattr(vp,'SCREEN-X')-(getattr(vp,'WIDTH')/2)) for vp in sceneSubset.visPoints if vp.groupIdx==groupIdx]))
            groupRE = int(max([(getattr(vp,'SCREEN-X')+(getattr(vp,'WIDTH')/2)) for vp in sceneSubset.visPoints if vp.groupIdx==groupIdx]))
            groupTE = int(min([(getattr(vp,'SCREEN-Y')-(getattr(vp,'HEIGHT')/2)) for vp in sceneSubset.visPoints if vp.groupIdx==groupIdx]))
            groupBE = int(max([(getattr(vp,'SCREEN-Y')+(getattr(vp,'HEIGHT')/2)) for vp in sceneSubset.visPoints if vp.groupIdx==groupIdx]))
        
            # apply padding and scaling factors
            groupLE = (groupLE-groupPad)*scaleFactor
            groupRE = (groupRE+groupPad)*scaleFactor
            groupTE = (groupTE-groupPad)*scaleFactor
            groupBE = (groupBE+groupPad)*scaleFactor
            
            groupColor = colors[colorList[colorIdx]]
    
            # "draw" left edge
            img2write[groupTE:groupBE,(groupLE-lineWidth):(groupLE+lineWidth),:] = groupColor
            # "draw" right edge
            img2write[groupTE:groupBE,(groupRE-lineWidth):(groupRE+lineWidth),:] = groupColor
            # "draw" top edge
            img2write[(groupTE-lineWidth):(groupTE+lineWidth),groupLE:groupRE,:] = groupColor
            # "draw" bottom edge
            img2write[(groupBE-lineWidth):(groupBE+lineWidth),groupLE:groupRE,:] = groupColor
            
        colorIdx += 1
        groupPad += 6
        
    #prep folder/filename
    if not os.path.isdir(saveFolder+'_'.join(radii)+'px/'):
        saveFolder = saveFolder+'_'.join(radii)+'px/'
        os.mkdir(saveFolder)
    else:
        saveFolder = saveFolder+'_'.join(radii)+'px/'
    
    #write image
    cv2.imwrite(saveFolder+stimName+'.jpg',img2write)
    
        
        
def mk_grpFeatModelPred_jpgs(scene, stimName, saveFolder, lineWidth=5, groupPad=12, scaleFactor=3):
    
    img2write = np.full((3300,2550,3),255,dtype=np.uint8)
    
    colorList = ['red','green','blue','pink','orange','cyan','purple']
    colors = {'red':[np.uint8(0),np.uint8(0),np.uint8(255)],
              'green':[np.uint8(0),np.uint8(255),np.uint8(0)],
              'blue':[np.uint8(255),np.uint8(0),np.uint8(0)],
              'pink':[np.uint8(255),np.uint8(0),np.uint8(255)],
              'orange':[np.uint8(0),np.uint8(165),np.uint8(255)],
              'cyan':[np.uint8(255),np.uint8(255),np.uint8(0)],
              'purple':[np.uint8(128),np.uint8(0),np.uint8(128)]}
    
    # first, put the stimulus objects into the image - only need to do this once
    for vp in scene.visPoints:
        if vp.ISA[1] != 'GROUP':
            
            xCoord = getattr(vp,'SCREEN-X')
            yCoord = getattr(vp,'SCREEN-Y')
            height = getattr(vp,'HEIGHT')
            width = getattr(vp,'WIDTH')
        
            # there is a scaling factor - should be 3?
            xCoord = xCoord*scaleFactor
            yCoord = yCoord*scaleFactor
            height = height*scaleFactor
            width = width*scaleFactor
        
            # compute edges - x/y coords are centered on object
            leftEdge = int(xCoord-(width/2))
            rightEdge = int(xCoord+(width/2))
            topEdge = int(yCoord-(height/2))
            bottomEdge = int(yCoord+(height/2))
            
    
    
            # "draw" left edge
            img2write[topEdge:bottomEdge,(leftEdge-lineWidth):(leftEdge+lineWidth),:] = np.uint8(0)
            # "draw" right edge
            img2write[topEdge:bottomEdge,(rightEdge-lineWidth):(rightEdge+lineWidth),:] = np.uint8(0)
            # "draw" top edge
            img2write[(topEdge-lineWidth):(topEdge+lineWidth),leftEdge:rightEdge,:] = np.uint8(0)
            # "draw" bottom edge
            img2write[(bottomEdge-lineWidth):(bottomEdge+lineWidth),leftEdge:rightEdge,:] = np.uint8(0)
            
    # second, put the group labelings into the image
    for groupingIter in range(1,scene.groupGroupingIters+1):
        
        groupColor = colors[colorList[groupingIter-1]]
        
        for vp in scene.visPoints:
            if vp.groupGroupingIter == groupingIter:
            
                xCoord = getattr(vp,'SCREEN-X')
                yCoord = getattr(vp,'SCREEN-Y')
                height = getattr(vp,'HEIGHT')
                width = getattr(vp,'WIDTH')
        
                # compute edges - x/y coords are centered on object
                leftEdge = int(xCoord-(width/2))
                rightEdge = int(xCoord+(width/2))
                topEdge = int(yCoord-(height/2))
                bottomEdge = int(yCoord+(height/2))
            
                leftEdge = (leftEdge-groupPad)*scaleFactor
                rightEdge = (rightEdge+groupPad)*scaleFactor
                topEdge = (topEdge-groupPad)*scaleFactor
                bottomEdge = (bottomEdge+groupPad)*scaleFactor
    
    
                # "draw" left edge
                img2write[topEdge:bottomEdge,(leftEdge-lineWidth):(leftEdge+lineWidth),:] = groupColor
                # "draw" right edge
                img2write[topEdge:bottomEdge,(rightEdge-lineWidth):(rightEdge+lineWidth),:] = groupColor
                # "draw" top edge
                img2write[(topEdge-lineWidth):(topEdge+lineWidth),leftEdge:rightEdge,:] = groupColor
                # "draw" bottom edge
                img2write[(bottomEdge-lineWidth):(bottomEdge+lineWidth),leftEdge:rightEdge,:] = groupColor
                
        groupPad += 6
            
    #write image
    cv2.imwrite(saveFolder+stimName+'.jpg',img2write)
    
            
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
