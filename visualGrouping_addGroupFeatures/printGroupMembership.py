


for idx in range(0,len(vgConfig.vgScene.visPoints)):
    
    if hasattr(vgConfig.vgScene.visPoints[idx],'VALUE'):
        print('Visual feature ' + 
              vgConfig.vgScene.visPoints[idx].visiconID +
              ' which is a group, id: ' +
               vgConfig.vgScene.visPoints[idx].VALUE +
              ', belongs to group id: ' +
              vgConfig.vgScene.visPoints[idx].groupName + 
              ' and was grouped on iteration: ' + 
              str(vgConfig.vgScene.visPoints[idx].groupGroupingIter) +
              '.\n'
              )
    else:
        print('Visual feature ' + 
              vgConfig.vgScene.visPoints[idx].visiconID +
              ' which is a '+
              getattr(vgConfig.vgScene.visPoints[idx],'ISA')[0] +
              '; ' +
              getattr(vgConfig.vgScene.visPoints[idx],'ISA')[1] +
              ', belongs to group id: ' +
              vgConfig.vgScene.visPoints[idx].groupName +
              ' and was grouped on iteration: ' + 
              str(vgConfig.vgScene.visPoints[idx].groupGroupingIter) +
              '.\n'
              )
    