# Copyright 2021 Werner Mueller
# Released under the GPL (>= 2)

library(tensorflow)
library(Rcpp)
Sys.setenv("PKG_CXXFLAGS"="-std=c++17")
sourceCpp("src/dmInt.cpp")

evalEnvironment <- new.env(parent = emptyenv())
evalEnvironment$denseNetwork <- NULL

#' Reset API
#'
#' Reset API
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' dmReset()}
dmReset <- function() {
  if(!is.null(evalEnvironment$denseNetwork)) {
    evalEnvironment$denseNetwork <- NULL
  }
  dmResetSub()
}

dmReadSub <- function(generativeDataFileName, datModelFileName) {
  dmGenerativeDataRead(generativeDataFileName)
  dmReadDataModel(datModelFileName)
}

readDenseNetwork <- function(dataModelFileName) {
    numberOfHiddenLayerUnits = dmDataModelGetNumberOfHiddenLayerUnits()
    numberOfHiddenLayerUnits <- as.integer(numberOfHiddenLayerUnits)

    discriminatorHiddenLayer1 <- tf$keras$layers$Dense(units = numberOfHiddenLayerUnits, activation = tf$nn$leaky_relu)
    discriminatorHiddenLayer2 <- tf$keras$layers$Dense(units = numberOfHiddenLayerUnits, activation = tf$nn$leaky_relu)
    discriminatorLogits <- tf$keras$layers$Dense(units = 1L)

    checkPoint <- tf$train$Checkpoint(discriminatorHiddenLayer1 = discriminatorHiddenLayer1,
                                      discriminatorHiddenLayer2 = discriminatorHiddenLayer2,
                                      discriminatorLogits = discriminatorLogits)

    checkPoint$read(dmGetFileName(dataModelFileName))

    discriminatorNetwork <- tf_function(function(input) {
        discriminatorHiddenLayer1 <- discriminatorHiddenLayer1(input)
        discriminatorHiddenLayer2 <- discriminatorHiddenLayer2(discriminatorHiddenLayer1)
        logits <- discriminatorLogits(discriminatorHiddenLayer2)

        list(discriminatorHiddenLayer1, discriminatorHiddenLayer2, logits)
    })

    evalEnvironment$denseNetwork <- discriminatorNetwork
    evalEnvironment$denseNetwork
}

#' Read a data model and generative data
#'
#' Read a data model and generative data from files.
#' This function has to be called before calling API functions when
#' file names for a data model and gernerative data are not passed to functions directly.
#'
#' @param dataModelFileName Name of data model file
#' @param generativeDataFileName Name of generative data file
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' dmRead("dm.bin", "gd.bin")}
dmRead <- function(dataModelFileName, generativeDataFileName) {
  if(!is.null(evalEnvironment$denseNetwork)) {
    evalEnvironment$denseNetwork <- NULL
  }
  dmReadSub(generativeDataFileName, dataModelFileName);
  readDenseNetwork(dataModelFileName)
  return()
}

dmEvaluate <- function(data, withDenseLayers = FALSE) {
  if(is.null(evalEnvironment$denseNetwork)) {
    message("No data model")
    return()
  }

  batchSize <- dmGetBatchSize()
  dimension <-dmGetDataSourceDimension()
  data <- array_reshape(data, c(batchSize, dimension))

  if(!withDenseLayers) {
    e <- evalEnvironment$denseNetwork(data)
    e <- e[[3]]
  } else {
    e <- evalEnvironment$denseNetwork(data)
  }
  e
}

#' Calculate a density value for a data record
#'
#' Calculate a density value for a data record by evaluating the contained neural network in a data model.
#'
#' @param dataRecord List containing a data record
#'
#' @return Normalized density value
#' @export
#'
#' @examples
#' \dontrun{
#' dmRead("dm.bin", "gd.bin")
#' dmCalculateDensityValue(list(4.4, 2.9, 1.4, 0.2))}
dmCalculateDensityValue <- function(dataRecord) {
  dimension <-dmGetDataSourceDimension()
  batchSize <- dmGetBatchSize()
  if(length(dataRecord) != dimension) {
    message("Invalid dimension")
    return()
  }

  normalizedDataRow <- dmNormalizedDataRecord(dataRecord)
  data <- array(0, c(batchSize, dimension))
  for(i in 1:dimension) {
    data[1, i] = normalizedDataRow[i]
  }

  e <- dmEvaluate(data)
  e <- as.numeric(e[1, 1])
  if(e < 0) {
      e <- 0
  }
  e
}

dmEvaluateDataSource <- function(evaluateDataSourceFileName) {
  dmEvaluateDataSourceRead(evaluateDataSourceFileName)

  evaluateRowsSize <- dmGetEvaluateCopyDataSourceNormalizedSize()
  evaluateRows <- array(0, c(evaluateRowsSize))

  batchSize <- dmGetBatchSize()

  i <- 1
  while(i <= dmGetEvaluateCopyDataSourceNormalizedSize()) {
    data <- dmEvaluateCopyDataSourceGetNormalizedData(i, batchSize)
    w <- dmEvaluate(data)

    for(j in 0:(batchSize - 1)) {
      if(i + j <= evaluateRowsSize) {
        evaluateRows[i + j] = w[1 + j]
      }
    }
    i <- i + batchSize
  }
  evaluateRows
}

