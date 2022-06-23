

processCodedSheets = function(filepaths) {
  
  # get the path to save the plots to
  savePath = sapply(strsplit(filepaths[1],'matrices'), `[`,1)
  
  # get the names of the coders
  coderNames = sapply(strsplit(filepaths,'_'), `[`,3)
  coderNames = unlist(strsplit(coderNames,'.xlsx'))
  
  # get names of sheets for each xlsx in filepaths - want to make sure the same participants are compared
  sheetNames = lapply(filepaths,excel_sheets)
  
  # remove "BLANK" from the sheetnames
  for (i in 1:length(sheetNames)) {
    sheetNameList = sheetNames[[i]]
    sheetNameList = sheetNameList[-1]
    # fix the sheetnames
    #https://stackoverflow.com/questions/33683862/first-entry-from-string-split
    #sheetNameList = sapply(strsplit(sheetNameList,'_'), `[`,1)
    sheetNames[[i]] = sheetNameList
  }
  
  # make the plots
  for (i in 1:length(sheetNames[[1]])) {
    sheetNameList = c(sheetNames[[1]][i],sheetNames[[2]][i],sheetNames[[3]][i])
    compareStimCoding(filepaths,sheetNameList,savePlotPath=savePath,checkFiles=FALSE)
  }
  
}

filepaths = c('/home/ausmanpa/Desktop/stimCoding/stimuli/stim02/matrices_stim02_PATRICK.xlsx',
              '/home/ausmanpa/Desktop/stimCoding/stimuli/stim02/matrices_stim02_ARIANA.xlsx',
              '/home/ausmanpa/Desktop/stimCoding/stimuli/stim02/matrices_stim02_LINDA.xlsx')
processCodedSheets(filepaths)

filepaths = c('/home/ausmanpa/Desktop/stimCoding/stimuli/stim03/matrices_stim03_PATRICK.xlsx',
              '/home/ausmanpa/Desktop/stimCoding/stimuli/stim03/matrices_stim03_ARIANA.xlsx',
              '/home/ausmanpa/Desktop/stimCoding/stimuli/stim03/matrices_stim03_LINDA.xlsx')
processCodedSheets(filepaths)
