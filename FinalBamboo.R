install.packages("raster")
library(raster)

Band5Directory <- "C:/Users/Pooki/Documents/R/BambooBand5"
#compile working directory pathway

AllBandsList <- file.path(Band5Directory, paste0("bamboo_", 1984:2018, ".tif"))
#only files with .tif

Band5Stack <- stack(lapply(AllBandsList, raster, band = 5))
#extract only band 5 from each raster image and stack them together

adjacentNew <- function(Band5Stack, cellNrs){
  rowColCells <- rowColFromCell(Band5Stack, cellNrs)
  rowColRight <- rowColCells + matrix(rep(c(0,1),each=length(cellNrs)), ncol=2)
  rowColBottom <- rowColCells + matrix(rep(c(1,0),each=length(cellNrs)), ncol=2)
  
  cellRight <- cellFromRowCol(Band5Stack, rowColRight[,1], rowColRight[,2])
  cellBottom <- cellFromRowCol(Band5Stack, rowColBottom[,1], rowColBottom[,2])
  
  out <- matrix(c(cellNrs, cellNrs, cellRight, cellBottom), ncol=2, byrow=FALSE)
  out <- out[order(out[,1]),]
  return(out[!is.na(rowSums(out)),])
}

adjacentNew(Band5Stack, 1:ncell(Band5Stack))
# to create table of adjacent pairs of combinations

band5Values <- values(Band5Stack)
# to load values into memory

AdjTable <- adjacentNew(Band5Stack, 1:ncell(Band5Stack))
#create large matrix of all elements

plot(band5Values[1,], band5Values[2,])
#to plot values next to each other to see correlation pattern on graph

Band5Cor <- apply(AdjTable, 1, function(x){cor(band5Values[x[1],],band5Values[x[2],],use="complete.obs")})
#to calculate band 5 correlations


