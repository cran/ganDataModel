# Copyright 2021 Werner Mueller
# Released under the GPL (>= 2)

library(tensorflow)
library(Rcpp)

Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
sourceCpp("src/dmInt.cpp")
source("R/dmEvaluate.R")

dmAddVolumeElements <- function(level) {
  batchSize <- dmGetBatchSize()
  
  i <- 1
  while(i <= dmGetNormalizedSize()) {
    generativeData <- dmGenerativeDataGetNormalizedData(i, batchSize)
    r <- dmEvaluate(generativeData, TRUE)
  
    dl1Dimension <- dim(r[[1]])[2]
    dl2Dimension <- dim(r[[2]])[2]
    dl3Dimension <- dim(r[[3]])[2]
    dl4Dimension <- dim(r[[4]])[2]
    outDimension <- dim(r[[5]])[2]
    
    dimension <- dl1Dimension + dl2Dimension + dl3Dimension + dl4Dimension + outDimension
    dimensions <- c(dl1Dimension, dl2Dimension, dl3Dimension, dl4Dimension, outDimension)
    
    volumeElementValues <- array(0, c(dimension * batchSize))
    j <- 1
    for(j in 1:batchSize) {
      begin <- (j - 1) * dimension + 1
      end <- j * dimension
      volumeElementValues[begin:end] <- c(r[[1]][j,], r[[2]][j,], r[[3]][j,], r[[4]][j,], r[[5]][j,])
    }
  
    if(dmGetNormalizedSize() - i + 1 < batchSize) {
      volumeElementValues  <- volumeElementValues [1:(dimension * (dmGetNormalizedSize() - i + 1))]
    }
    dmAddVolumeElementsSub(volumeElementValues, dimensions, i, level)

    i <- i + batchSize
  }
}

#' Build subspaces for a data model for a level
#' 
#' Read a data model and generative data from files,
#' analyse the trained neural network in a data model for a level,
#' determine subspaces with density values above the level,
#' add obtained subspaces with assigned level to the data model
#' and write it to original file.
#'
#' @param dataModelFileName Name of data model file
#' @param level Level
#' @param generativeDataFileName Name of generative data file
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' dmBuildSubspaces("dm.bin", 0.73, "gd.bin")}
dmBuildSubspaces <- function(dataModelFileName, level, generativeDataFileName) {
  dmReset()
  dmRead(dataModelFileName, generativeDataFileName)
  
  dmProgress("Step 1 of 3", dmGetNormalizedSize())
  dmAddVolumeElements(level)
  dmBuildVolumeElements()
  
  dmProgress("Step 2 of 3")
  dmBuildVolumeElementTree()
  
  dmProgress("Step 3 of 3")
  dmBuildVolumeElementGraph()
  dmBuildSubspacesSub()
  dmAddVolumeElementGraph()
  dmWrite(dataModelFileName)  
}

#' Remove subspaces from a data model for a level
#' 
#' Read a data model from file,
#' remove subspaces from the data model for a level
#' and write it to original file.
#'
#' @param dataModelFileName Name of data model file
#' @param level Level
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' dmRead("dm.bin", "gd.bin")
#' dmRemoveSubspaces("dm.bin", 0.73)}
dmRemoveSubspaces <- function(dataModelFileName, level) {
  dmReset()
  dmReadDataModel(dataModelFileName)
  dmRemoveSubspacesSub(level)
  dmWrite(dataModelFileName)  
}

#' Get subspaces in a data model to which a data record is assigned
#' 
#' Determine to which pairs of level and enumerated subspace a data record is assigned.
#'
#' @param dataRecord List of a data record
#'
#' @return List of pairs of level and enumerated subspace
#' @export
#'
#' @examples
#' \dontrun{
#' dmRead("dm.bin", "gd.bin")
#' dmGetAssignedSubspaces(list(4.4, 2.9, 1.4, 0.3))}
dmGetAssignedSubspaces <- function(dataRecord) {
  l <- dmCalculateDensityValue(dataRecord)
  levelSubspaces <- dmGetSubspacesSub(dataRecord, l)
  levelSubspaces
}
