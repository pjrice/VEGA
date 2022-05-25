library(readxl)
library(tidyr)

# argument should be a list of files we are comparing
filepaths = c('/home/ausmanpa/Desktop/stimCodingComp/Patrick/p017_s15_v1.xlsx',
              '/home/ausmanpa/Desktop/stimCodingComp/Mike/mdbMatrix_s15.xlsx',
              '/home/ausmanpa/Desktop/stimCodingComp/Linda/p017_s15.xlsx')


# check that we are comparing the same files
fnames = sapply(strsplit(filepaths,'/'),tail,1)
if (!length(unique(fnames))==1) {
  stop('Files in argument list have different filenames')
}

# load files and concatenate into dataframe
for (file in filepaths) {
  
  tempData = read_excel(file,sheet=1)
  
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
  
  if (!exists('compData')) {
    compData = tempData
  } else {
    compData = rbind(compData,tempData)
  }
  
}


# compare each cell across coders
# https://stackoverflow.com/questions/15933958/collapse-concatenate-aggregate-a-column-to-a-single-comma-separated-string-w
compData %>% 
  group_by(obj1,obj2,coder) %>% 
  summarise(uniqueGroups=unique(grouping)) %>%
  group_by(obj1,obj2,uniqueGroups) %>%
  summarise(test = toString(coder)) %>%
  ungroup()

















