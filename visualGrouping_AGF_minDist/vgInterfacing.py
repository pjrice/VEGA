import threading
import vgClasses
import vgLabeling
import vgConfig

##############################################################################
# ACT-R interfacing

# ensure that "group" is a valid slot name for visicon chunks
vgLabeling.actr.extend_possible_slots("group", warn=False)
        
# also have to extend visicon chunk slots with left/right/top/bottom entries
vgLabeling.actr.extend_possible_slots("screen-left",warn=False)
vgLabeling.actr.extend_possible_slots("screen-right",warn=False)
vgLabeling.actr.extend_possible_slots("screen-top",warn=False)
vgLabeling.actr.extend_possible_slots("screen-bottom",warn=False)



# If the model is reset, clear the list of features
def reset ():

    vgConfig.features = []
    vgConfig.currentVisicon = []
    vgConfig.vgScene = None
    vgConfig.modelIsVoting = False
    
    
   
# add this function as an actr command, so that it can be monitored
vgLabeling.actr.add_command("modvis-reset",reset)

# monitor the actr reset-start signal, which is generated by the reset command
# unlike clear-all, reset clears the event cue and restores the current set of
# models and modules to their initial states (clear-all removes all models and
# modules)
# a monitor is used to have a command evaluated automatically when another
# command is evaluated - so, the "mod-vis" reset command (which calls the 
# reset() function defined above) is evaluated when the reset-start signal
# is generated
# When this is converted to an actr module, instead of monitoring reset-start, 
# it should be called as the reset function for the module
vgLabeling.actr.monitor_command("reset-start","modvis-reset")


# This function will be monitoring the add-visicon-features
# command as an after monitor which means it will be passed
# four parameters: 
#  the name of the command that was called
#  the list of parameters passed to that command
#  a boolean that indicates whether the call completed successfully
#  a list of the return values from the command if it was successful
# Because :force-visual-commands is enabled, all calls to add-visual-features
# (from the remote interfaces as well as internally to actr) will use the actr
# "command" version (always the case for the remote interface, not always for 
# internal actr, when parameter is not enabled) so that it can be monitored.
# Otherwise, some internal actr calls would use the underlying function, and
# not be caught by the monitor
def features_added(cmd,params,success,results):
    # 'results' parameter contains the visicon feature IDs returned by add-visicon-features
    # 'params' parameter contains the parameters used by the add-visicon-features call
    
    # Store the feature ids in the features list
    vgConfig.features = vgConfig.features + results[0]
    
    # insert the feature ID into the feature's details 
    [j.insert(0,i) for i,j in zip(results[0],params)]
    
    for idx in range(len(params)):
        
        #get x/y coordinate of feature
        screenXVal = params[idx][params[idx].index('SCREEN-X') + 1]
        screenYVal = params[idx][params[idx].index('SCREEN-Y') + 1]
        
        # height/width may not be defined in the (add-visicon-features) call
        # if not, assume height/width is 1 and update actr feature to reflect this
        if ('HEIGHT' in params[idx]):
            heightVal = params[idx][params[idx].index('HEIGHT') + 1]
        else:
            heightVal = 1
            params[idx] = params[idx]+['HEIGHT',heightVal]
            vgConfig.modVisLock = True
            vgLabeling.actr.modify_visicon_features([results[0][idx],'HEIGHT',heightVal])
            vgConfig.modVisLock = False
            
        if ('WIDTH' in params[idx]):
            widthVal = params[idx][params[idx].index('WIDTH') + 1]
        else:
            widthVal = 1
            params[idx] = params[idx]+['WIDTH',widthVal]
            vgConfig.modVisLock = True
            vgLabeling.actr.modify_visicon_features([results[0][idx],'WIDTH',widthVal])
            vgConfig.modVisLock = False
            
        # compute the left/right/top/bottom boundaries that are used by the models
        boundaries = vgLabeling.compute_boundaries(screenXVal,screenYVal,heightVal,widthVal)
        
        # modify the visicon entry for this feature with the computed boundaries
        vgConfig.modVisLock = True
        vgLabeling.actr.modify_visicon_features([results[0][idx],"screen-left",boundaries["SCREEN-LEFT"],
                                                              "screen-right",boundaries["SCREEN-RIGHT"],
                                                              "screen-top",boundaries["SCREEN-TOP"],
                                                              "screen-bottom",boundaries["SCREEN-BOTTOM"]])
        vgConfig.modVisLock = False
        
        # update the params list with the calculated boundaries
        boundsList = [val for pair in zip(boundaries.keys(),boundaries.values()) for val in pair]
        params[idx] = params[idx]+boundsList
        
    vgConfig.currentVisicon = vgConfig.currentVisicon + params
    if vgConfig.noImages:
        vgConfig.noImageVisicon = []
        vgConfig.noImageVisicon = [feat for feat in vgConfig.currentVisicon if feat[feat.index('ISA')+1][1]!='IMAGE']
        
    
    

# add this function as an actr command, so that it can be monitored    
vgLabeling.actr.add_command("modvis-add",features_added)

# actr.py doesn't support after and before monitoring since it's not
# used in the tutorial.  So this depends on the internals of that
# code to send the appropriate message which makes it unsafe for
# compatibility.
# so, will implement actr.monitor_command_before and actr.monitor_command_after
# methods
vgLabeling.actr.current_connection.interface.send("monitor","add-visicon-features","modvis-add","after")

# Monitor for the modification of features
def features_modified(cmd,params,success,results):
    
    if vgConfig.modVisLock:
        pass
    else:
        # find the indices of the modified features in the currentVisicon
        modFeatIdxs = []
        for f in results[0]:
            modFeatIdxs.append(next(i for i,v in enumerate(vgConfig.currentVisicon) if f in v))
            
        
        # update vgConfig.currentVisicon to reflect the modifications to the modified features
        for i in range(len(modFeatIdxs)):
            
            # vgConfig.currentVisicon index of the feature we're modifying
            cvIdx = modFeatIdxs[i]
            
            # list of the attributes that were modified for this feature - this includes added and deleted attributes
            moddedAttrs = params[i][1::2]
            
            # list of the new values of the modified attributes - this includes added and deleted attribute values
            newAttrVals = params[i][2::2]
            
            # go through the vgConfig.currentVisicon feature representation and update with new values
            for moddedAttrPair in zip(moddedAttrs,newAttrVals):
                # determine if the modified attribute already exists in the feature or if it's new
                if moddedAttrPair[0] in vgConfig.currentVisicon[cvIdx]:
                    # since attr already exists,get index of the value of the modified attribute 
                    # of the feature that was modified
                    maIdx = vgConfig.currentVisicon[cvIdx].index(moddedAttrPair[0])+1
                    # update attribute with new value
                    if (moddedAttrPair[0]=='HEIGHT' or moddedAttrPair[0]=='WIDTH') and moddedAttrPair[1] is None:
                        # height or width attr was removed, so instead assume a value of 1 as in features_added()
                        vgConfig.currentVisicon[cvIdx][maIdx] = 1
                        # update ACT-R visicon with this assumption
                        modVisArgList = [vgConfig.currentVisicon[cvIdx][0],moddedAttrPair[0],1]
                        threading.Thread(target=vgLabeling.call_modify_visicon_features, args=(modVisArgList,), daemon=True).start()
                    else:
                        vgConfig.currentVisicon[cvIdx][maIdx] = moddedAttrPair[1]
                else:
                    # attribute is new, so append it
                    vgConfig.currentVisicon[cvIdx].append(moddedAttrPair[0])
                    vgConfig.currentVisicon[cvIdx].append(moddedAttrPair[1])
                    
            # if X/Y/height/width were changed, recompute screen-left/right/top/bottom and update
            if any(item in moddedAttrs for item in ['SCREEN-X','SCREEN-Y','HEIGHT','WIDTH']):
                
                xValIdx = vgConfig.currentVisicon[cvIdx].index('SCREEN-X')+1
                yValIdx = vgConfig.currentVisicon[cvIdx].index('SCREEN-Y')+1
                heightValIdx = vgConfig.currentVisicon[cvIdx].index('HEIGHT')+1
                widthValIdx = vgConfig.currentVisicon[cvIdx].index('WIDTH')+1
                
                screenXVal = vgConfig.currentVisicon[cvIdx][xValIdx]
                screenYVal = vgConfig.currentVisicon[cvIdx][yValIdx]
                heightVal = vgConfig.currentVisicon[cvIdx][heightValIdx]
                widthVal = vgConfig.currentVisicon[cvIdx][widthValIdx]
                
                # compute the left/right/top/bottom boundaries that are used by the models
                boundaries = vgLabeling.compute_boundaries(screenXVal,screenYVal,heightVal,widthVal)
                
                # update the vgConfig.currentVisicon representation with the new values
                for attr in ['SCREEN-LEFT','SCREEN-RIGHT','SCREEN-TOP','SCREEN-BOTTOM']:
                    attrIdx = vgConfig.currentVisicon[cvIdx].index(attr)+1
                    vgConfig.currentVisicon[cvIdx][attrIdx] = boundaries[attr]
                
                # modify the visicon entry for this feature with the computed boundaries
                # has to be done in another thread otherwise this will just hang waiting for the 
                # modify-visicon-features call that triggered this to finish
                modVisArgList = [vgConfig.currentVisicon[cvIdx][0],
                                 "screen-left",boundaries["SCREEN-LEFT"],
                                 "screen-right",boundaries["SCREEN-RIGHT"],
                                 "screen-top",boundaries["SCREEN-TOP"],
                                 "screen-bottom",boundaries["SCREEN-BOTTOM"]]
                threading.Thread(target=vgLabeling.call_modify_visicon_features, args=(modVisArgList,), daemon=True).start()
                
        
    

vgLabeling.actr.add_command("modvis-mod",features_modified)
vgLabeling.actr.current_connection.interface.send("monitor","modify-visicon-features","modvis-mod","after")

# Similar monitors for the removal of features
def features_removed(cmd,params,success,results):
    # 'results' parameter contains the visicon feature IDs returned by delete-visicon-features
    
    for f in results[0]:
        vgConfig.features.remove(f)
        
        featIdx = [x for x in range(len(vgConfig.currentVisicon)) if vgConfig.currentVisicon[x][0]==f]
        del vgConfig.currentVisicon[featIdx[0]]
    
    # calling (clear-all) in ACT-R seems to recursively call delete-visicon-features to remove everything
    # so to clear the modelIsVoting flag, check for the length of features/currentVisicon to be zero
    if len(vgConfig.features)==0 and len(vgConfig.currentVisicon)==0:
        vgConfig.modelIsVoting = False
    
        

vgLabeling.actr.add_command("modvis-remove",features_removed)
vgLabeling.actr.current_connection.interface.send("monitor","delete-visicon-features","modvis-remove","after")


def all_features_removed():
    
    vgConfig.features = []
    vgConfig.currentVisicon = []

vgLabeling.actr.add_command("modvis-remove-all",all_features_removed)
vgLabeling.actr.monitor_command("delete-all-visicon-features","modvis-remove-all")

def proc_display_monitor(cmd,params,success,results):
    
    try:
        
        if not vgConfig.modelIsVoting:
        
            # using the list of current visicon features in currentVisicon, group
            # the scene using the specified radius and collision method
            # radius=20 for No-Lines-Color; "used to" be 25? (still discrepancies; some party affiliations are not grouped)
            # radius=8 for No-Lines-Color-Box
            if vgConfig.noImages:
                vgConfig.vgScene = vgClasses.visGroups(vgConfig.noImageVisicon,vgConfig.collisionMethod)
            else:
                vgConfig.vgScene = vgClasses.visGroups(vgConfig.currentVisicon,vgConfig.collisionMethod)

            # display boxes around the visicon content
            if vgConfig.denoteGroups:
                vgLabeling.denote_group_extent(vgConfig.vgScene)
                
            vgConfig.modelIsVoting = True
                
        
        
        
            # now that groups have been determined and we've done everything we
            # want with them, store the current scene as the previous scene
            #vgConfig.vgPrevScene = vgConfig.vgScene
        
        
    except AttributeError:
        raise

    
    
vgLabeling.actr.add_command("pd-mon",proc_display_monitor)
vgLabeling.actr.current_connection.interface.send("monitor","proc-display","pd-mon","before")
