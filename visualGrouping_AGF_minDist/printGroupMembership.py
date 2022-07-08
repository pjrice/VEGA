

for idx in range(0,len(vgConfig.vgScene.visPoints)):
    
    if hasattr(vgConfig.vgScene.visPoints[idx],'VALUE'):
        print('Visual feature ' + 
              vgConfig.vgScene.visPoints[idx].visiconID +
              '\ntype: ' +
               vgConfig.vgScene.visPoints[idx].VALUE +
              '\ngroup id: ' +
              vgConfig.vgScene.visPoints[idx].groupName + 
              '\ngrouping iteration: ' + 
              str(vgConfig.vgScene.visPoints[idx].groupGroupingIter) +
              '\nheight: ' +
              str(vgConfig.vgScene.visPoints[idx].HEIGHT) +
              '\nscreen-top: ' +
              str(getattr(vgConfig.vgScene.visPoints[idx],'SCREEN-TOP')) +
              '\nscreen-bottom: ' +
              str(getattr(vgConfig.vgScene.visPoints[idx],'SCREEN-BOTTOM')) +
              '.\n'
              )
    else:
        print('Visual feature ' + 
              vgConfig.vgScene.visPoints[idx].visiconID +
              '\ntype: '+
              getattr(vgConfig.vgScene.visPoints[idx],'ISA')[0] +
              '; ' +
              getattr(vgConfig.vgScene.visPoints[idx],'ISA')[1] +
              '\ngroup id: ' +
              vgConfig.vgScene.visPoints[idx].groupName + 
              '\ngrouping iteration: ' + 
              str(vgConfig.vgScene.visPoints[idx].groupGroupingIter) +
              '\nheight: ' +
              str(vgConfig.vgScene.visPoints[idx].HEIGHT) +
              '\nscreen-top: ' +
              str(getattr(vgConfig.vgScene.visPoints[idx],'SCREEN-TOP')) +
              '\nscreen-bottom: ' +
              str(getattr(vgConfig.vgScene.visPoints[idx],'SCREEN-BOTTOM')) +
              '.\n'
              )
        
        
screenTops = [x[x.index('SCREEN-TOP')+1] for x in vgConfig.currentVisicon]
screenBottoms = [x[x.index('SCREEN-BOTTOM')+1] for x in vgConfig.currentVisicon]        

tbDiff = [i-j for i,j in zip(screenBottoms,screenTops)]