
import random
import numpy as np
import matplotlib.pyplot as plt


#xVals = list(range(1,len(candActDict.keys())+1))

xVals = list()
yVals = list()
yErr = list()
colors = list()
xVal = 1
color = 'g'
for raceNum in candActDict.keys():
    
    if candActDict[raceNum]:
        for candidate in candActDict[raceNum].keys():
            xVals.append(xVal+round(random.uniform(-0.5,0.5),2))
            yVals.append(np.mean(candActDict[raceNum][candidate]))
            yErr.append(np.std(candActDict[raceNum][candidate]))
            colors.append(color)
            
    else:
        # these were all retrieval failures, add one xVal to list and 0 to yVal list
        
        xVals.append(xVal)
        yVals.append(0)
        yErr.append(0)
        colors.append(color)
        
    xVal += 1
    if color=='g':
        color = 'r'
    elif color=='r':
        color = 'b'
    else:
        color = 'g'
            
            
    




# https://matplotlib.org/stable/gallery/lines_bars_and_markers/errorbar_limits_simple.html#sphx-glr-gallery-lines-bars-and-markers-errorbar-limits-simple-py


fig = plt.figure()

plt.axhline(0,color='black')
plt.scatter(xVals,yVals,color=colors)
plt.errorbar(xVals,yVals,yerr=yErr,ls='none',ecolor=colors)
plt.xticks(np.arange(1,max(xVals),1))
plt.xlabel('Contest number')
plt.ylabel('Mean activation at retrieval')
plt.title('Mean activation value across 100 runs for each candidate of each contest')










x1Values = ['alice', 'jordan', 'mike']
y1Values = [(2,3),(1,1),(4,5)]

xData = []
yData = []
cols = []
for counter, name in enumerate(x1Values):
    #Decide which point gets which color
    if (y1Values[counter][0] > y1Values[counter][1]):
        cols += ['r', 'g']
    elif (y1Values[counter][0] < y1Values[counter][1]):
        cols += ['g', 'r']
    else:
        cols += ['b', 'b']

    #adding names and values to the data for x and y Axes    
    xData += [name, name]
    yData += y1Values[counter]