# Copyright 2021 Werner Mueller
# Released under the GPL (>= 2)

library(tensorflow)
library(Rcpp)

Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
sourceCpp("src/dmInt.cpp")
source("R/dmEvaluate.R")

dmPlot <- function(title, dimension, columnIndices) {
  numberVectorIndexNames <- dmGetNumberVectorIndexNames(columnIndices)
  minX <- dmGetMin(columnIndices[1])
  maxX <- dmGetMax(columnIndices[1])
  minY <- NULL
  maxY <- NULL
  if(dimension == 1) {
    minY <- 0
    maxY <- 1
  } else {
    minY <- dmGetMin(columnIndices[2])
    maxY <- dmGetMax(columnIndices[2])
  }
  
  plot(c(), c(), pch = 1, main = title, cex.main = 2.5, font.main = 1, xlim = c(minX, maxX), ylim = c(minY, maxY), col = "blue", xlab = numberVectorIndexNames[1], ylab = numberVectorIndexNames[2], cex.lab = 2.5, cex.axis = 2.5)
}

dmGenerativeDataPoints <- function(percent, batchSize, dimension, columnIndices, level, gdColour) {
  data <- dmGetGenerativeDataRandom(percent);
  size <- length(data[[1]]) / dimension
  
  i <- 1
  while(i <= size) {
    s <- batchSize
    if(batchSize > size - i + 1) {
      s <- size - i + 1
    }

    nd <- array(0, c(batchSize * dimension))
    nds <- s * dimension
    ndb <- (i - 1) * dimension + 1
    nd[1:nds] <- data[[1]][ndb:(ndb + nds - 1)]
    nd <- array_reshape(nd, c(batchSize, dimension))
    
    d <- dmEvaluate(nd)
    
    dd <- array(0, c(batchSize * dimension))
    dd[1:nds] <- data[[2]][ndb:(ndb + nds - 1)]
    dd <- array_reshape(dd, c(batchSize, dimension))
    
    if(dimension != 1) {
      points(dd[1:s, columnIndices[1]], dd[1:s, columnIndices[2]], pch = 1, col = ifelse(d >= level, gdColour, rgb(0, 0, 0, alpha = 0.0)), cex = 1.0)
    } else {
      ddY <- array(0, c(s))
      points(dd[1:s, columnIndices[1]], ddY[1:s], pch = 1, col = ifelse(d >= level, gdColour, rgb(0, 0, 0, alpha = 0.0)), cex = 1.0)      
    }
    
    i <- i + batchSize
  }
}

dmSubspacePoints <- function(level, subspace, percent, boundary, batchSize, dimension, columnIndices, dmSubspaceColour) {
  data <- dmGetSubspaceDenormalizedGenerativeData(level, subspace, percent, boundary)
  size <- length(data) / dimension
  
  if(size > 0) {
    data <- array_reshape(data, c(size, dimension))
  
    if(dimension != 1) {
      points(data[1:size, columnIndices[1]], data[1:size, columnIndices[2]], pch = 1, col = dmSubspaceColour, cex = 1.0)
    } else {
      dataY <- array(0, c(size))
      points(data[1:size, columnIndices[1]], dataY[1:size], pch = 1, col = dmSubspaceColour, cex = 1.0)
    }
  }
}

dmEvaluateDataSourcePoints <- function(level, batchSize, dimension, columnIndices, evaluateDataSourceColour, greaterEqual = TRUE) {
  i <- 1
  while(i <= dmGetEvaluateCopyDataSourceNormalizedSize()) {
    size <- batchSize
    if(batchSize > dmGetEvaluateCopyDataSourceNormalizedSize() - i + 1) {
      size <- dmGetEvaluateCopyDataSourceNormalizedSize() - i + 1
    }
    
    data <- dmGetEvaluateCopyDataSourceNormalizedData(i, batchSize)
    nd <- array_reshape(data[[1]], c(batchSize, dimension))
    dd <- array_reshape(data[[2]], c(batchSize, dimension))
    
    d <- dmEvaluate(nd)
    
    if(dimension != 1) {
      if(greaterEqual) {
        points(dd[1:size, columnIndices[1]], dd[1:size, columnIndices[2]], pch = 1, col = ifelse(d >= level, evaluateDataSourceColour, rgb(0, 0, 0, alpha = 0.0)), cex = 1.1)
      } else {
        points(dd[1:size, columnIndices[1]], dd[1:size, columnIndices[2]], pch = 1, col = ifelse(d < level, evaluateDataSourceColour, rgb(0, 0, 0, alpha = 0.0)), cex = 2.7)      
      }
    } else {
      ddY <- array(0, c(size))
      if(greaterEqual) {
        points(dd[1:size, columnIndices[1]], ddY[1:size], pch = 1, col = ifelse(d >= level, evaluateDataSourceColour, rgb(0, 0, 0, alpha = 0.0)), cex = 1.1)
      } else {
        points(dd[1:size, columnIndices[1]], ddY[1:size], pch = 1, col = ifelse(d < level, evaluateDataSourceColour, rgb(0, 0, 0, alpha = 0.0)), cex = 2.7)      
      }
    }
    
    i <- i + batchSize
  }
}

#' Specify plot parameters for generative data
#' 
#' Specify plot parameters for generative data passed to function dmPlotSubspaces().
#'
#' @param percent Percentr of randomly selected data points of generative data
#' @param colour Colour for data points of generative data
#' 
#' @return List of plot parameters for generative data
#' @export
#' 
#' @examples
#' \dontrun{
#' dmPlotGenerativeDataParameters(5)}
dmPlotGenerativeDataParameters <- function(percent = 10, colour = "red") {
  parameters <- list(percent = percent, colour = colour)
}

#' Specify plot parameters for subspaces in a data model
#' 
#' Specify plot parameters for subspaces in a data model passed to function dmPlotSubspaces().
#'
#' @param percent Percent of randomly selected data points of generative data assigned to subspaces
#' @param boundary Boolean value indicating if only data points of subspace boundaries should be selected
#' @param colour Colour for data points of generative data assigned to subspaces
#' 
#' @return List of plot parameters for subspaces
#' @export
#' 
#' @examples
#' \dontrun{
#' dmPlotSubspaceParameters(50)}
dmPlotSubspaceParameters <- function(percent = 100, boundary = TRUE, colour = "green") {
  parameters <- list(percent = percent, boundary = boundary, colour = colour)
}

#' Specify plot parameters for evaluated data source
#' 
#' Specify plot parameters for evaluated data source passed to function dmPlotSubspaces().
#'
#' @param colour Colour for data points of evaluaded data source
#' 
#' @return List of plot parameters for evaluated data source
#' @export
#' 
#' @examples
#' \dontrun{
#' dmPlotEvaluateDataSourceParameters()}
dmPlotEvaluateDataSourceParameters <- function(colour = "blue") {
  parameters <- list(colour = colour)
}

dmLegend <- function(level, enumeratedSubspaces,
                     plotGenerativeDataParameters,
                     plotSubspaceParameters,
                     plotEvaluateDataSourceParameters) {
  lPchVector <- c()
  lLegendVector <- c()
  lColVector <- c()
  lPt <- c()
  
  if(!is.null(plotGenerativeDataParameters)) {
    lPchVector <- c(lPchVector, 1)
    lLegend <- "generative data"
    lLegendVector <- c(lLegendVector, lLegend)
    lColVector <- c(lColVector, plotGenerativeDataParameters[[2]])
    lPt <- c(lPt, c(1.0))
  }
  
  if(!is.null(plotSubspaceParameters)) {
    lPchVector <- c(lPchVector, 1)
    
    lLegend <- NULL
    if(length(enumeratedSubspaces) != 1) {
      lLegend <- "subspaces"
    } else {
      lLegend <- "subspace"
    }
    
    for(subspaceIndex in enumeratedSubspaces) {
      lLegend <- paste(lLegend, " ", subspaceIndex, sep = "")
    }
    lLegend <- paste(lLegend, ", density level = ", level, sep = "")
    lLegend <- paste(lLegend, ", generative data", sep = "")
    
    lLegendVector <- c(lLegendVector, lLegend)
    lColVector <- c(lColVector, plotSubspaceParameters[[3]])
    lPt <- c(lPt, c(1.0))
  }
  
  if(!is.null(plotEvaluateDataSourceParameters)) {
    lPchVector <- c(lPchVector, c(1, 1))
    lLegend <- paste("evaluated data source,", "density value >=", level)
    lLegendVector <- c(lLegendVector, lLegend)
    lLegend <- paste("evaluated data source,", "density value <", level)
    lLegendVector <- c(lLegendVector, lLegend)
    lColVector <- c(lColVector, c(plotEvaluateDataSourceParameters[[1]], plotEvaluateDataSourceParameters[[1]]))
    lPt <- c(lPt, c(1.1, 2.7))
  }
  
  legend("topleft", legend = lLegendVector, col = lColVector, pch = lPchVector, horiz = FALSE, cex = 2.0, pt.cex = lPt, bty = 'n')
}

dmPng <- function(level, enumeratedSubspaces, imageFileName, title, columnIndices,
                  plotGenerativeDataParameters,
                  plotSubspaceParameters,
                  evaluateDataSourceFileName,
                  plotEvaluateDataSourceParameters,
                  dimension) {
  png(imageFileName, width = 2000, height = 2000, units = "px")
  
  sessionPar <- par(no.readonly = TRUE)
  on.exit(par(sessionPar))
  par(mar = c(6, 6, 6, 6))
  
  dmPlot(title, dimension, columnIndices)
  
  batchSize <- dmGetBatchSize()
  if(!is.null(plotGenerativeDataParameters)) {
    dmGenerativeDataPoints(plotGenerativeDataParameters[[1]], batchSize, dimension, columnIndices, 0, plotGenerativeDataParameters[[2]])
  }
  
  if(!is.null(plotSubspaceParameters)) {
    for(subspace in enumeratedSubspaces) {
      dmSubspacePoints(level, subspace, plotSubspaceParameters[[1]], plotSubspaceParameters[[2]], batchSize, dimension, columnIndices, plotSubspaceParameters[[3]])
    }
  }
  
  if(evaluateDataSourceFileName != 0 && !is.null(plotEvaluateDataSourceParameters)) {
    dmEvaluateDataSourceRead(evaluateDataSourceFileName)
    dmEvaluateDataSourcePoints(level, batchSize, dimension, columnIndices, plotEvaluateDataSourceParameters[[1]], TRUE)
    dmEvaluateDataSourcePoints(level, batchSize, dimension, columnIndices, plotEvaluateDataSourceParameters[[1]], FALSE)
  }

  dmLegend(level,
           enumeratedSubspaces,
           plotGenerativeDataParameters,
           plotSubspaceParameters,
           plotEvaluateDataSourceParameters)
  
  dev.off()
  
  return()
}

#' Create an image file for built subspaces
#' 
#' Create an image file containing two-dimensional projections of generative data, 
#' gernerative data assigned to subspaces and optionally an evaluated data source.
#' Plot parameters are passed by functions dmPlotGenerativeDataParameters(),
#' dmPlotSubspaceParameters(), dmPlotEvaluateDataSourceParameters().
#' Data points are drwan in the order generative data, generative data assigned to built subspaces
#' and evaluated data source.
#'
#' @param level Level for subspaces
#' @param enumeratedSubspaces Vector of enumerated subspaces
#' @param imageFileName Name of image file
#' @param title Title of image
#' @param columnIndices Vector of two column indices that are used for the two-dimensional projection.
#' Indices refer to indices of active columns of data source.
#' @param plotGenerativeDataParameters Plot generative data parameters, see dmPlotGenerativeDataParameters().
#' @param plotSubspaceParameters Plot parameters for subspaces, see dmPlotSubspaceParameters().
#' @param evaluateDataSourceFileName Name of evaluated data source file
#' @param plotEvaluateDataSourceParameters Plot parameters for evaluated data source, see dmPlotEvaluateDataSourceParameters().
#' 
#' @return None
#' @export
#' 
#' @examples
#' \dontrun{
#' dmRead("dm.bin", "gd.bin")
#' dmPlotSubspaces(0.73,
#'   c(1),
#'   "s1d34.png",
#'   "Subspace of Hierarchical, Categorical Data Model for the Iris Dataset",
#'   c(3, 4),
#'   dmPlotGenerativeDataParameters(10),
#'   dmPlotSubspaceParameters(100),
#'   "ds.bin",
#'   dmPlotEvaluateDataSourceParameters())}
dmPlotSubspaces <- function(level, enumeratedSubspaces, imageFileName, title, columnIndices, 
  plotGenerativeDataParameters = dmPlotGenerativeDataParameters(percent = 10, colour = "red"),
  plotSubspaceParameters = dmPlotSubspaceParameters(percent = 100, colour = "green"),
  evaluateDataSourceFileName = "",
  plotEvaluateDataSourceParameters = NULL) {

  dimension <- dmGetDataSourceDimension()
  if(dimension > 1 && length(columnIndices) != 2) {
    message("Size of vector columnIndices must be equal to two\n")
    return()
  } else if (dimension == 1 && length(columnIndices) != 1) {
    message("Size of vector columnIndices must be equal to one\n")
    return()
  }
  if(dimension >= 1) {
    if(columnIndices[1] < 1 || columnIndices[1] > dimension) {
      message(paste("Column indices must be in the range of 1 to", dimension, "\n"))
      return()
    }
    if(dimension >= 2) {
      if(columnIndices[2] < 1 || columnIndices[2] > dimension) {
        message(paste("Column indices must be in the range of 1 to", dimension, "\n"))
        return()
      }
    }
  }
  
  dmPng(level, enumeratedSubspaces, imageFileName, title, columnIndices, 
        plotGenerativeDataParameters,
        plotSubspaceParameters,
        evaluateDataSourceFileName,
        plotEvaluateDataSourceParameters,
        dimension)
  
  return()
}

#' Create an image file for an evaluated data source
#' 
#' Create an image file containing two-dimensional projections of generative data
#' and an evaluated data source.
#' Plot parameters are passed by functions dmPlotGenerativeDataParameters()
#' and dmPlotEvaluateDataSourceParameters().
#' Data points are drwan in the order generative data, evaluated data source.
#
#' @param evaluateDataSourceFileName Name of evaluated data source file'
#' @param level Level for subspaces
#' @param imageFileName Name of image file
#' @param title Title of image
#' @param columnIndices Vector of two column indices that are used for the two-dimensional projection.
#' Indices refer to indices of active columns of data source.
#' @param plotGenerativeDataParameters Plot generative data parameters, see dmPlotGenerativeDataParameters().
#' @param plotEvaluateDataSourceParameters Plot parameters for evaluated data source, see dmPlotEvaluateDataSourceParameters().
#' 
#' @return None
#' @export
#' 
#' @examples
#' \dontrun{
#' dmRead("dm.bin", "gd.bin")
#' dmRead("dm.bin", "gd.bin")
#' dmPlotEvaluate("ds.bin",
#'   0.73,
#'   "s1d34.png",
#'   "Subspace of Hierarchical, Categorical Data Model for the Iris Dataset",
#'   c(3, 4),
#'   dmPlotGenerativeDataParameters(10),
#'   dmPlotEvaluateDataSourceParameters())}
dmPlotEvaluate <- function(evaluateDataSourceFileName, level, imageFileName, title, columnIndices,
                           plotGenerativeDataParameters = dmPlotGenerativeDataParameters(percent = 10, colour = "red"),
                           plotEvaluateDataSourceParameters = dmPlotEvaluateDataSourceParameters(colour = "blue")) {
  dmPlotSubspaces(level, enumeratedSubspaces = c(), imageFileName, title, columnIndices,
                  plotGenerativeDataParameters,
                  plotSubspaceParameters = NULL,
                  evaluateDataSourceFileName,
                  plotEvaluateDataSourceParameters)
}
