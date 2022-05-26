library(readxl)
library(tidyr)
library(dplyr)

sortGroupingLabels = function(groupingLabel) {
  if (is.na(groupingLabel)) {
    return(NA)
  } else {
    orderedLabel = paste(sort(unlist(strsplit(groupingLabel,''))),sep='',collapse='')
    return(orderedLabel)
  }
}

getPairLabels = function(filepaths,obj1idx,obj2idx,checkFiles=TRUE) {
  
  # check that we are comparing the same files
  if (checkFiles) {
    fnames = sapply(strsplit(filepaths,'/'),tail,1)
    if (!length(unique(fnames))==1) {
      stop('Files in argument list have different filenames')
    }
  }
  
  # load files and concatenate into dataframe
  for (file in filepaths) {
    
    tempData = read_excel(file,sheet=1)
    
    # remove obj_idx column
    tempData = subset(tempData,select=-obj_idx)
    
    # get group label at specified indices
    groupLabel = tempData[obj1idx,obj2idx]
    
    # get name of coder
    coderName = tail(strsplit(file,'/')[[1]],2)[1]
    
    tempDF = data.frame(coderName,groupLabel)
    colnames(tempDF) = c('coder','label')
    
    if (!exists('labelDF')) {
      labelDF = tempDF
    } else {
      labelDF = rbind(labelDF,tempDF)
    }
    
  }
  
  # make sure the labels are in alphabetical order for comparison between coders
  labelDF$label = sapply(labelDF$label,sortGroupingLabels)
  
  labelDF = labelDF %>% 
    group_by(label) %>%
    summarise(coder=toString(coder))
  
  return(labelDF)
  
}