
scene = vgConfig.vgScene

for vp in scene.visPoints:
    print('Object type: '+vp.ISA[1])
    print('  -screen-x: '+str(getattr(vp,'SCREEN-X')))
    print('  -screen-y: '+str(getattr(vp,'SCREEN-Y')))
    print('  -width: '+str(getattr(vp,'WIDTH')))
    print('  -height: '+str(getattr(vp,'HEIGHT')))
    print('  -screen-top: '+str(getattr(vp,'SCREEN-TOP')))
    print('  -screen-bottom: '+str(getattr(vp,'SCREEN-BOTTOM')))
    print('  -screen-left: '+str(getattr(vp,'SCREEN-LEFT')))
    print('  -screen-right: '+str(getattr(vp,'SCREEN-RIGHT')))
    print('  -grouping iter:'+str(getattr(vp,'groupGroupingIter')))