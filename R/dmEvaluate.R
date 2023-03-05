# Copyright 2021 Werner Mueller
# Released under the GPL (>= 2)

library(tensorflow)
library(Rcpp)
Sys.setenv("PKG_CXXFLAGS"="-std=c++17")
sourceCpp("src/dmInt.cpp")

evalEnvironment <- new.env(parent = emptyenv())
evalEnvironment$session <- list()

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
  if(length(evalEnvironment$session) > 0) {
    evalEnvironment$session[[1]]$close()
    evalEnvironment$session <- list()
  }
  dmResetSub()
}

dmReadSub <- function(generativeDataFileName, datModelFileName) {
  dmGenerativeDataRead(generativeDataFileName)
  dmReadDataModel(datModelFileName)
}

loadSession <- function(dataModelFileName) {
  gdTf <- tf$compat$v1
  gdTf$disable_v2_behavior()
 
  metaGraphFileName <- dmBuildFileName(dataModelFileName, "meta")
  dataModelFileName <- dmBuildFileName(dataModelFileName, "")
 
  graph <- gdTf$Graph()
  with (graph$as_default(), {
    session <- gdTf$Session(graph = graph)
    loader = gdTf$train$import_meta_graph(metaGraphFileName, clear_devices = TRUE)
    loader$restore(session, dataModelFileName)
  })   
  
  x <- graph$get_tensor_by_name('Placeholder:0')
  y <- graph$get_tensor_by_name('Placeholder_1:0')
  
  evalEnvironment$session[[1]] <- session
  evalEnvironment$session[[2]] <- x
  
  denseLayer1 <- graph$get_tensor_by_name('Identity:0')
  denseLayer2 <- graph$get_tensor_by_name('Identity_1:0')
  denseLayer3 <- graph$get_tensor_by_name('Identity_2:0')
  denseLayer4 <- graph$get_tensor_by_name('Identity_3:0')
  logits <- graph$get_tensor_by_name('Identity_4:0')
  
  
  evalEnvironment$session[[3]] <- denseLayer1
  evalEnvironment$session[[4]] <- denseLayer2
  evalEnvironment$session[[5]] <- denseLayer3
  evalEnvironment$session[[6]] <- denseLayer4
  evalEnvironment$session[[7]] <- logits
  
  evalEnvironment$session
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
  if(length(evalEnvironment$session) > 0) {
    evalEnvironment$session[[1]]$close()
  }
  dmReadSub(generativeDataFileName, dataModelFileName);
  loadSession(dataModelFileName)
  return()
}

dmEvaluate <- function(data, withDenseLayers = FALSE) {
  if(length(evalEnvironment$session) == 0) {
    message("No data model")
    return()
  }
  
  batchSize <- dmGetBatchSize()
  dimension <-dmGetDataSourceDimension()
  
  w <- array(0, c(batchSize))
  
  s <- evalEnvironment$session[[1]]
  x <- evalEnvironment$session[[2]]
  dl1 <- evalEnvironment$session[[3]]
  dl2 <- evalEnvironment$session[[4]]
  dl3 <- evalEnvironment$session[[5]]
  dl4 <- evalEnvironment$session[[6]]
  logits <- evalEnvironment$session[[7]]
  
  data <- array_reshape(data, c(batchSize, dimension))
  
  if(!withDenseLayers) {
    e <- s$run(list(logits), feed_dict = dict(x = data))
    e <- array_reshape(e[[1]], c(batchSize))
  } else {
    e <- s$run(list(dl1, dl2, dl3, dl4, logits), feed_dict = dict(x = data))
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
#' dmCalculateDensityValue(list(4.4, 2.9, 1.4, 0.3))}
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
  e <- e[1]
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
