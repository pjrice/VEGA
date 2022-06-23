library(readxl)
library(tidyr)
library(dplyr)

collapseDiagonal = function(path2file) {
  
  # load data
  data = read_excel(path2file,sheet=1)
  
  # for object pairs that were not grouped, replace NA with 'w'
  # (performed for top half/triangle of matrix only)
  for (rIdx in 1:nrow(data)) {
    for (cIdx in (rIdx+1):ncol(data)) {
      if (is.na(data[rIdx,cIdx])) {
        data[rIdx,cIdx] = 'w'
      }
    }
  }
  
  # drop obj_idx column
  data = data %>% select(-obj_idx)
  
  # for each cell on the diagonal, want to collapse unique values in the column and row the diag cell belongs to
  for (idx in 1:nrow(data)) { #matrices are square in this case
    
    rowUnique = unique(data[idx,])
    colUnique = unique(data[,idx])
    
  }
}
