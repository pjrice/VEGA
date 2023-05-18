import json


#open CCL, load model

#load python VEGA

numRuns = 100
candActDict = {}
for runNum in range(0,numRuns):
    
    print('Run number: '+str(runNum))
    
    # start the retrieval history
    vgLabeling.actr.record_history("retrieval-history")
    
    # run the model using the run-model-from-python command, defined in combine.lisp
    vgLabeling.actr.current_connection.evaluate("run-model-from-python")
    
    # stop the history recording after the model is done, and get the data from the history
    vgLabeling.actr.stop_recording_history("retrieval-history")
    rawRetHist = vgLabeling.actr.get_history_data("retrieval-history")
    
    # the history data is stored as a json
    retHist = json.loads(rawRetHist)
    
    raceNum = 1
    for i in range(0,len(retHist)): # for each retrieval in the history
        
        # if this was a "contest" retrieval to test for an abstain vote, skip
        if retHist[i][1][4] == 'C':
            pass
        else:
            if ('Race'+str(raceNum)) not in candActDict.keys(): # building a nested dictionary
                candActDict['Race'+str(raceNum)] = {}
                
            # get the time in milliseconds of the retrieval
            retrievalTime = retHist[i][0]
            
            for ii in range(0,len(retHist[i][2])): # for each chunk that was eligible for the current retrieval   
                candName = retHist[i][2][ii][0] # get the candidate name for the current chunk
                
                if candName == ':RETRIEVAL-FAILURE': # if the retrieval failed, skip
                    pass
                else:
                    if candName not in candActDict['Race'+str(raceNum)].keys():
                        candActDict['Race'+str(raceNum)][candName] = list()
                    
                        # get the activation value of the current chunk
                    activationVal = float(retHist[i][2][ii][3].split("has an activation of: ")[-1].split('\n')[0])
                    candActDict['Race'+str(raceNum)][candName].append(activationVal)
                    
            raceNum += 1
    
    # reload the model using the reload-model-from-python command, defined in combine.lisp 
    # NOTE: YOU NEED TO MAKE SURE THE CORRECT MODEL IS SPECIFIED IN THIS COMMAND       
    vgLabeling.actr.current_connection.evaluate("reload-model-from-python")
    
                
                
            
            
                
            