
import matplotlib.pyplot as plt






def draw_dendrogram(groupedScene):
    
    #plot original features first
    origFeats = [x for x in groupedScene.visPoints if not hasattr(x,'VALUE')]
    
    plt.scatter([x for x in range(0,len(origFeats))], [0 for x in range(0,len(origFeats))])
    plt.show()
    