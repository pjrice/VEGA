import math

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


def weighted_collision(argDict):
    
    pWeight = 1
    vWeight = 1
    hWeight = 1
    aWeight = 1
    iWeight = 1
    
    
    p1 = argDict['point1']
    p2 = argDict['point2']
    
    # get relevant attributes of the two points
    p1x = getattr(p1,'SCREEN-X')
    p1y = getattr(p1,'SCREEN-Y')
    p1h = getattr(p1,'HEIGHT')
    p1w = getattr(p1,'WIDTH')
    p1v = getattr(p1,'VALUE')
    p1i = getattr(p1,'ISA')[1]
    p2x = getattr(p2,'SCREEN-X')
    p2y = getattr(p2,'SCREEN-Y')
    p2h = getattr(p2,'HEIGHT')
    p2w = getattr(p2,'WIDTH')
    p2v = getattr(p1,'VALUE')
    p2i = getattr(p2,'ISA')[1]
    
    # compute proximity (as euclidean distance)
    # closer points should be more likely to be grouped together
    proximity = xy_euclidian_distance((p1x,p1y),(p2x,p2y))
    
    # compute vertical and horizontal alignment
    # points that are more aligned should be more likely to be grouped together
    # compute vertical alignment (absolute value of difference in x value)
    vAlignment = abs(p1x - p2x)
    # compute horizontal alignment (absolute value of difference in y value)
    hAlignment = abs(p1y - p2y)
    
    # compute area
    # points that are similar in area should be more likely to be grouped together
    # absolute value of difference in area between the two points
    p1a = p1h*p1w
    p2a = p2h*p2w
    area = abs(p1a-p2a)
    
    # what to do with value?
    
    # compute identity correspondence
    # binary - are the two points the same "thing"? 
    identity = int(p1i==p2i)
    
    weightedDist = ((pWeight*proximity) + 
                    (vWeight*vAlignment) + 
                    (hWeight*hAlignment) + 
                    (aWeight*area) +
                    (iWeight*identity))
    
    return(weightedDist)
    

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
    
    # defines a box around p1
    p1x = getattr(p1,'SCREEN-X')
    p1y = getattr(p1,'SCREEN-Y')
    p1w = getattr(p1,'WIDTH')
    p1h = getattr(p1,'HEIGHT')
        
    leftEdge = p1x - (p1w/2)
    rightEdge = p1x + (p1w/2)
    topEdge = p1y - (p1h/2)
    bottomEdge = p1y + (p1h/2)
    
    
    # in John's words: "if there was a target, check it, otherwise we are overlapping and just return T"
    if target:
        
        tx = target[0]
        ty = target[1]
        
        # if the target point's x position is within the left and right edges of point1's box, and the target point's y position is within the top/bottom edges padded by the given radius,
        # it's a hit vertically. hitHorz is the same vice-versa: within the top/bottom edge, and within the left/right edges padded by the given radius
        hitVert = (tx >= leftEdge) and (tx <= rightEdge) and (ty >= (topEdge-radius)) and (ty <= (bottomEdge+radius))
        hitHorz = (tx >= (leftEdge-radius)) and (tx <= (rightEdge+radius)) and (ty >= topEdge) and (ty <= bottomEdge)
        
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
    
    targetWidth = getattr(targetPt,'WIDTH')
    targetHeight = getattr(targetPt,'HEIGHT')
    targetX = getattr(targetPt,'SCREEN-X')
    targetY = getattr(targetPt,'SCREEN-Y')
        
    leftEdge = targetX - (targetWidth/2)
    rightEdge = targetX + (targetWidth/2)
    topEdge = targetY - (targetHeight/2)
    bottomEdge = targetY + (targetHeight/2)
    
    # first four if statements check if origin point is outside of left/right
    # edges AND top/bottom edges;
    # second four if statements check if origin point is OUTSIDE of left/right
    # edge and WITHIN the top/bottom edges, or vice-versa
    if (originX < leftEdge) and (originY > bottomEdge):
        closestPt = (leftEdge,bottomEdge)
    elif (originX < leftEdge) and (originY < topEdge):
        closestPt = (leftEdge,topEdge)
    elif (originX > rightEdge) and (originY > bottomEdge):
        closestPt = (rightEdge,bottomEdge)
    elif (originX > rightEdge) and (originY < topEdge):
        closestPt = (rightEdge,topEdge)
    elif (originX < leftEdge) and (originY <= bottomEdge) and (originY >= topEdge):
        closestPt = (leftEdge, originY)
    elif (originX > rightEdge) and (originY <= bottomEdge) and (originY >= topEdge):
        closestPt = (rightEdge,originY)
    elif (originY < topEdge) and (originX >= leftEdge) and (originX <= rightEdge):
        closestPt = (originX,topEdge)
    elif (originY > bottomEdge) and (originX >= leftEdge) and (originX <= rightEdge):
        closestPt = (originX,bottomEdge)
    else:
        closestPt = False
        
    return(closestPt)
        
            

def determine_pt2pt_collision(mode,argDict):
    switcher = {
        'point':point_collision,
        'box':box_collision,
        'weighted':weighted_collision
        }
    
    func = switcher.get(mode, lambda: "Invalid collision method")
    return(func(argDict))
