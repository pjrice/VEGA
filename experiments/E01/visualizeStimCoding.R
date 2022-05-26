library(readxl)
library(tidyr)
library(ggplot2)
library(data.table)

# https://stackoverflow.com/questions/38506324/split-fill-in-ggplot-geom-tile-or-heatmap-two-colors-by-third-value
# https://stackoverflow.com/questions/22107666/generating-split-color-rectangles-from-ggplot2-geom-raster
# https://cran.r-project.org/web/packages/data.table/data.table.pdf
# https://stackoverflow.com/questions/48452193/dual-color-rectangles-from-ggplot2-geom-raster?noredirect=1&lq=1

visualizeStimCoding = function(path2file) {
  
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
  
  # convert data to long format
  longData = data %>% gather(key='obj_idx2',value='grouping',-obj_idx)
  colnames(longData) = c('obj1','obj2','grouping')

  # for any object pair that belongs to more than one group, need to split into separate rows
  longData = as.data.table(longData)
  longData = longData[, strsplit(as.character(grouping),split=''),by=list(obj1,obj2)] # splits groups of characters into separate rows for each character
  longData[, shift:=(1:(.N))/.N - 1/(2 * .N) - 1/2, by=list(obj1,obj2)] #computes y-value shift for any split values (0 otherwise)
  longData[, height:=1/.N, by=list(obj1,obj2)] # computes height of box for any split values (1 otherwise)
  longData$gridLabel = paste(longData$obj1,longData$obj2,sep=', ') # make obj1,obj2 labels for grid cells
  colnames(longData) = c('obj1','obj2','grouping','shift','height','gridLabel')
  
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
  
  # create plot
  ggplot(longData,aes(x=obj2,y=obj1+shift,fill=grouping,height=height)) + 
    geom_tile(color='black') + 
    scale_y_reverse(expand=c(0,0)) +
    scale_x_discrete(expand=c(0,0),limits=seq(1,nrow(data))) +
    scale_fill_manual(values=groupColors,labels=groupLabels) +
    geom_text(aes(label=gridLabel))
  
  
}