library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)

sortGroupingLabels = function(groupingLabel) {
  if (is.na(groupingLabel)) {
    return(NA)
  } else {
    orderedLabel = paste(sort(unlist(strsplit(groupingLabel,''))),sep='',collapse='')
    return(orderedLabel)
  }
}

compareStimCoding = function(filepaths,labelGrid=FALSE,checkFiles=TRUE,delim=', ') {
  
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
    
    # for object pairs that were not grouped, replace NA with 'w'
    # (performed for top half/triangle of matrix only)
    for (rIdx in 1:nrow(tempData)) {
      for (cIdx in (rIdx+1):ncol(tempData)) {
        if (is.na(tempData[rIdx,cIdx])) {
          tempData[rIdx,cIdx] = 'w'
        }
      }
    }
    
    # convert data to long format
    tempData = tempData %>% gather(key='obj_idx2',value='grouping',-obj_idx)
    colnames(tempData) = c('obj1','obj2','grouping')
    tempData$coder = tail(strsplit(file,'/')[[1]],2)[1]
    
    # make sure the labels are in alphabetical order for comparison between coders
    tempData$grouping = sapply(tempData$grouping,sortGroupingLabels)
    
    if (!exists('compData')) {
      compData = tempData
    } else {
      compData = rbind(compData,tempData)
    }
    
  }
  
  # compare each cell across coders
  # https://stackoverflow.com/questions/15933958/collapse-concatenate-aggregate-a-column-to-a-single-comma-separated-string-w
  # compData = compData %>% 
  #   group_by(obj1,obj2,coder) %>% 
  #   summarise(uniqueGroups=unique(grouping)) %>% 
  #   group_by(obj1,obj2,uniqueGroups) %>% 
  #   summarise(coders = paste(substr(coder,1,1),collapse=delim)) %>% #switched from toString() to paste() to use collapse arg for \n
  #   mutate(shift=(1:n())/n() - 1/(2*n()) - 1/2,
  #          height=1/n())
  
  compData = compData %>% 
    group_by(obj1,obj2,coder) %>% 
    summarise(uniqueGroups=unique(grouping)) %>% 
    group_by(obj1,obj2,uniqueGroups) %>% 
    summarise(coders = paste(substr(coder,1,1),collapse=delim)) %>% #switched from toString() to paste() to use collapse arg for \n
    mutate(yshift=(1:n())/n() - 1/(2*n()) - 1/2,
           height=1/n()) %>%
    mutate(uniqueGroups=strsplit(as.character(uniqueGroups),split='')) %>%
    unnest(uniqueGroups) %>%
    group_by(obj1,obj2,coders) %>%
    mutate(xshift=(1:n())/n() - 1/(2*n()) - 1/2,
           width=1/n())
  
  compData$obj2 = as.numeric(compData$obj2)
  
  # map stimulus coding symbols to ggplot colors
  groupColors = c('NA'='grey35',
                  'w'='white',
                  'p'='pink',
                  'r'='red',
                  'o'='orange',
                  'g'='green',
                  'c'='cyan',
                  'b'='blue',
                  'u'='purple',
                  'k'='black',
                  'w'='white')
  
  # map stimulus coding symbols to legend labels
  groupLabels = c('w'='no grouping',
                  'p'='pink',
                  'r'='red',
                  'o'='orange',
                  'g'='green',
                  'c'='cyan',
                  'b'='blue',
                  'u'='purple',
                  'k'='black')
  
  p1 = ggplot(compData,aes(x=obj2+xshift,y=obj1+yshift,fill=uniqueGroups,height=height,width=width)) +
  geom_tile(color='black') +
  scale_y_reverse(expand=c(0,0)) +
  scale_x_discrete(expand=c(0,0),limits=seq(1,max(as.numeric(compData$obj2)))) +
  scale_fill_manual(values=groupColors,labels=groupLabels)
  #geom_text(aes(label=coders))
  
  if (labelGrid) {
    p1 = p1+geom_text(aes(label=coders))
  }
  
  p1
    
  
}


























