library(readxl)
library(tidyr)
library(dplyr)


sheetNames = c('p005_stim02','p005','p005')

filepaths = c('/home/ausmanpa/Desktop/stimCoding/stimuli/stim02/matrices_stim02_ARIANA.xlsx',
              '/home/ausmanpa/Desktop/stimCoding/stimuli/stim02/matrices_stim02_LINDA.xlsx',
              '/home/ausmanpa/Desktop/stimCoding/stimuli/stim02/matrices_stim02_PATRICK.xlsx')


for (idx in 1:length(filepaths)) {
  
  file = filepaths[idx]
  
  sheetName = sheetNames[idx]
  
  tempData = read_excel(file,sheet=sheetName)
  
  # for object pairs that were not grouped, replace NA with 'w'
  # (performed for top half/triangle of matrix only)
  for (rIdx in 1:nrow(tempData)) {
    for (cIdx in (rIdx+1):ncol(tempData)) {
      if (is.na(tempData[rIdx,cIdx])) {
        tempData[rIdx,cIdx] = 'w'
      }
    }
  }
  
  tempData = tempData %>% select(-obj_idx)
  
  if (!exists('compArray')) {
    compArray = array(data=NA,dim=c(nrow(tempData),ncol(tempData),length(filepaths)))
    compArray[,,idx] = as.matrix(tempData)
  } else {
    compArray[,,idx] = as.matrix(tempData)
  }
  
  
}


# determine if the matrices were filled out the "same way" across coders, minus a difference in color for one of the three

# first make sure indices of NAs are the same
compNAs = lapply(seq(dim(compArray)[3]),function(x)is.na(compArray[,,x])) # apply is.na to each 2d matrix along 3rd dimension of array and put the results in a list
all(sapply(compNAs,function(x)identical(x,compNAs[[1]]))) # apply identical() between first matrix in list and each matrix in list (yes we are comparing the first matrix to itself, oh well)

# next make sure indices of 'w' are the same
compDubs = lapply(seq(dim(compArray)[3]),function(x)compArray[,,x]=='w')
all(sapply(compDubs,function(x)identical(x,compDubs[[1]])))

# next check that cells that do not contain NA/'w' have the same length
compGroupsIdx = (!is.na(compArray) & compArray != 'w')
compGroups = lapply(seq(dim(compGroupsIdx)[3]),function(x)compGroupsIdx[,,x])

# https://stackoverflow.com/questions/61841229/subset-assignment-of-multidimensional-array-in-r














