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

#' Build metric subspaces for a level
#' 
#' Read a data model and generative data from files,
#' analyze the contained neural network in the data model for a level,
#' determine metric subspaces with density values above the level,
#' add obtained metric subspaces to the data model
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
#' dmBuildMetricSubspaces("dm.bin", 0.7, "gd.bin")}
dmBuildMetricSubspaces <- function(dataModelFileName, level, generativeDataFileName) {
  dmReset()

  dmRead(dataModelFileName, generativeDataFileName)

  dmProgress("Step 1 of 3", dmGetNormalizedSize())
  dmAddVolumeElements(level)
  dmBuildVolumeElements()
  
  dmProgress("Step 2 of 3")
  dmBuildVolumeElementTree()
  
  dmProgress("Step 3 of 3")
  dmBuildVolumeElementGraph()
  dmBuildMetricSubspacesSub()
  dmAddVolumeElementGraph()
  dmWrite(dataModelFileName)  
}

#' Remove metric subspaces for a level
#' 
#' Read a data model from file,
#' remove metric subspaces in the data model for a level
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
#' dmRemoveMetricSubspaces("dm.bin", 0.7)}
dmRemoveMetricSubspaces <- function(dataModelFileName, level) {
  dmReset()
  dmReadDataModel(dataModelFileName)
  dmRemoveMetricSubspacesSub(level)
  dmWrite(dataModelFileName)  
}

#' Get metric subspaces in which a data record is contained
#' 
#' Determine in which metric subspaces in a data model a data record is contained.
#'
#' @param dataRecord List of a data record
#'
#' @return List of list containing level and label of metric subspaces
#' @export
#'
#' @examples
#' \dontrun{
#' dmRead("dm.bin", "gd.bin")
#' dmGetContaineInMetricSubspaces(list(4.4, 2.9, 1.4, 0.3))}
dmGetContainedInMetricSubspaces <- function(dataRecord) {
  l <- dmCalculateDensityValue(dataRecord)
  levelMetricSubspaces <- dmGetMetricSubspacesSub(dataRecord, l)
  levelMetricSubspaces
}
