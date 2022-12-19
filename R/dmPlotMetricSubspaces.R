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
  if(dimension == 1) {TRUE
    minY <- 0
    maxY <- 1
  } else {
    minY <- dmGetMin(columnIndices[2])
    maxY <- dmGetMax(columnIndices[2])
  }
  
  plot(c(), c(), pch = 1, main = title, cex.main = 2.5, font.main = 1, xlim = c(minX, maxX), ylim = c(minY, maxY), col = "blue", xlab = numberVectorIndexNames[1], ylab = numberVectorIndexNames[2], cex.lab = 2.5, cex.axis = 2.5)
}

dmMetricSubspacePoints <- function(level, metricSubspaceIndex, percent, boundary, color, batchSize, dimension, columnIndices) {
  data <- dmGetMetricSubspaceDenormalizedGenerativeData(level,
                                                        metricSubspaceIndex,
                                                        percent,
                                                        boundary)
  size <- length(data) / dimension
  if(size > 0) {
    data <- array_reshape(data, c(size, dimension))
    
    if(dimension != 1) {
      points(data[1:size, columnIndices[1]], data[1:size, columnIndices[2]], pch = 1, col = color, cex = 1.0)
    } else {
      dataY <- array(0, c(size))
      points(data[1:size, columnIndices[1]], dataY[1:size], pch = 1, col = color, cex = 1.0)
    }
  }
}

dmMetricSubspacesPoints <- function(plotMetricSubspaceParameters, batchSize, dimension, columnIndices) {
    labels <- plotMetricSubspaceParameters$labels
    metricSubspaceIndices <- dmGetMetricSubspaceIndices(plotMetricSubspaceParameters$level, labels)
    
    for(metricSubspaceIndex in metricSubspaceIndices) {
      if(plotMetricSubspaceParameters$backgroundReset) {
        dmMetricSubspacePoints(plotMetricSubspaceParameters$level,
                               metricSubspaceIndex,
                               100,
                               FALSE,
                               "white",
                               batchSize,
                               dimension,
                               columnIndices)
      }
      
      if(plotMetricSubspaceParameters$backgroundPercent > 0) {
        dmMetricSubspacePoints(plotMetricSubspaceParameters$level,
                               metricSubspaceIndex,
                               plotMetricSubspaceParameters$backgroundPercent,
                               plotMetricSubspaceParameters$boundary,
                               plotMetricSubspaceParameters$backgroundColor,
                               batchSize,
                               dimension,
                               columnIndices)
      }
      
      dmMetricSubspacePoints(plotMetricSubspaceParameters$level,
                             metricSubspaceIndex,
                             plotMetricSubspaceParameters$percent,
                             plotMetricSubspaceParameters$boundary,
                             plotMetricSubspaceParameters$color,
                             batchSize,
                             dimension,
                             columnIndices)
    }
}

dmMetricSubspaceLabelPoints <- function(lPlotMetricSubspaceParameters, rPlotMetricSubspaceParameters, batchSize, dimension, columnIndices) {
  lLevel <- lPlotMetricSubspaceParameters$level
  rLevel <- -1
  if(!is.null(rPlotMetricSubspaceParameters$level)) {
    rLevel <- rPlotMetricSubspaceParameters$level
  }
  percent <- lPlotMetricSubspaceParameters$percent
  lLabels <- lPlotMetricSubspaceParameters$labels
  metricSubspacePoints <- dmMetricSubspaceLabelPointsSub(lLevel, rLevel, percent, columnIndices, lLabels)
  data <- metricSubspacePoints[[1]]
  labels <- metricSubspacePoints[[2]]
  
  size <- length(data) / dimension
  if(size > 0) {
    data <- array_reshape(data, c(size, dimension))
  
    if(lPlotMetricSubspaceParameters$plotLabels) {
      cex <- 3.0
      for(i in 1:length(labels)) {
        label <- labels[i]
        if(dimension != 1) {
          x <- data[i, columnIndices[1]]
          y <- data[i, columnIndices[2]]
          text(x, y, c(label), cex = cex)
        } else {
          x <- data[i, columnIndices[1]]
          y <- 0
          text(x, y, c(label), cex = cex)
        }
      }
    }
  }
}

dmEvaluateDataSourcePoints <- function(level, batchSize, dimension, columnIndices, evaluateDataSourceColor, greaterEqual = TRUE) {
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
        points(dd[1:size, columnIndices[1]], dd[1:size, columnIndices[2]], pch = 1, col = ifelse(d >= level, evaluateDataSourceColor, rgb(0, 0, 0, alpha = 0.0)), cex = 1.1)
      } else {
        points(dd[1:size, columnIndices[1]], dd[1:size, columnIndices[2]], pch = 1, col = ifelse(d < level, evaluateDataSourceColor, rgb(0, 0, 0, alpha = 0.0)), cex = 2.7)      
      }
    } else {
      ddY <- array(0, c(size))
      if(greaterEqual) {
        points(dd[1:size, columnIndices[1]], ddY[1:size], pch = 1, col = ifelse(d >= level, evaluateDataSourceColor, rgb(0, 0, 0, alpha = 0.0)), cex = 1.1)
      } else {
        points(dd[1:size, columnIndices[1]], ddY[1:size], pch = 1, col = ifelse(d < level, evaluateDataSourceColor, rgb(0, 0, 0, alpha = 0.0)), cex = 2.7)      
      }
    }
    
    i <- i + batchSize
  }
}

#' Specify plot parameters for metric subspaces for a level
#' 
#' Specify plot parameters for metric subspaces in a data model for a level.
#' A list of plot parameters is created for different levels and passed to dmPlotMetricSubspaces().
#'
#' @param level Level for metric subspaces.
#' @param labels Vector of labels for metric subspaces.
#' The default vector contains the wildcard character * which includes all labels. 
#' @param percent Percent of randomly selected data points of generative data contained in metric subspaces
#' @param boundary Boolean value indicating if only data points of metric subspace boundaries should be selected
#' @param color Color for data points of generative data contained in metric subspaces
#' @param backgroundPercent Percent of randomly selected data points of generative data contained in metric subspaces for background
#' @param backgroundColor Color for data points of generative data contained in metric subspaces for background
#' @param backgroundReset Before data points for a metric subspace are drawn reset its background.
#' @param plotLabels Boolean value indicating if labels for metric subspaces for a level should be displayed
#' 
#' @return List of plot parameters for metric subspaces
#' @export
#' 
#' @examples
#' \dontrun{
#' dmPlotMetricSubspaceParameters(0.73)}
dmPlotMetricSubspaceParameters <- function(level, labels = c("*"), percent = 10, boundary = TRUE, color = "red",
                                           backgroundPercent = 0, backgroundColor = "red", backgroundReset = TRUE, plotLabels = TRUE) {
  parameters <- list(level = level, labels = labels, percent = percent, boundary = boundary, color = color,
                     backgroundPercent = backgroundPercent, backgroundColor = backgroundColor, backgroundReset = backgroundReset, plotLabels = plotLabels)
}

#' Specify plot parameters for evaluated data source
#' 
#' Specify plot parameters for evaluated data source passed to dmPlotMetricSubspaces().
#'
#' @param level Level for evaluation
#' @param color Color for data points of evaluaded data source
#' 
#' @return List of plot parameters for evaluated data source
#' @export
#' 
#' @examples
#' \dontrun{
#' dmPlotEvaluateDataSourceParameters()}
dmPlotEvaluateDataSourceParameters <- function(level = 0, color = "blue") {
  parameters <- list(level = level, color = color)
}

dmLegend <- function(sortedLevelIndices,
                     plotMetricSubspaceParametersList,
                     plotEvaluateDataSourceParameters,
                     columnIndices) {
  lPchVector <- c()
  lLegendVector <- c()
  lColVector <- c()
  lPt <- c()
  
  if(!is.null(plotMetricSubspaceParametersList)) {
    lPchVector <- c(lPchVector, 1)
    
    for(sortedLevelIndex in sortedLevelIndices) {
      plotMetricSubspaceParameters <- plotMetricSubspaceParametersList[[sortedLevelIndex]]
      level <- plotMetricSubspaceParameters$level
      color <- plotMetricSubspaceParameters$color
      lLegend <- "labeled metric subspaces, generative data, "
      lLegend <- paste(lLegend, "level = ", level, sep = "")
      lLegendVector <- c(lLegendVector, lLegend)
      lColVector <- c(lColVector, color)
      lPt <- c(lPt, c(1.0))
    }
  }
  
  if(!is.null(plotEvaluateDataSourceParameters)) {
    lPchVector <- c(lPchVector, c(1, 1))
    lLegend <- paste("evaluated data source,", "density value >=", plotEvaluateDataSourceParameters[[1]])
    lLegendVector <- c(lLegendVector, lLegend)
    lLegend <- paste("evaluated data source,", "density value <", plotEvaluateDataSourceParameters[[1]])
    lLegendVector <- c(lLegendVector, lLegend)
    lColVector <- c(lColVector, c(plotEvaluateDataSourceParameters[[2]], plotEvaluateDataSourceParameters[[2]]))
    lPt <- c(lPt, c(1.0, 2.7))
  }

  legend("topleft", legend = lLegendVector, col = lColVector, pch = lPchVector, horiz = FALSE, cex = 2.5, pt.cex = lPt, bty = 'n')
}

dmGetSortedLevelIndices <- function(plotMetricSubspaceParametersList) {
  levels <- c()
  for(i in 1:length(plotMetricSubspaceParametersList)) {
    level <- plotMetricSubspaceParametersList[[i]]$level
    levels <- append(levels, level)
  }
  sortedLevelIndices <- dmSortLevelIndices(levels)
}

dmPng <- function(plotMetricSubspaceParametersList,
                  imageFileName,
                  title,
                  columnIndices,
                  evaluateDataSourceFileName,
                  plotEvaluateDataSourceParameters,
                  dimension) {
  png(imageFileName, width = 2000, height = 2000, units = "px")
  
  sessionPar <- par(no.readonly = TRUE)
  on.exit(par(sessionPar))
  par(mar = c(6, 6, 6, 6))
  
  dmPlot(title, dimension, columnIndices)
  
  batchSize <- dmGetBatchSize()
  if(!is.null(plotMetricSubspaceParametersList)) {
    sortedLevelIndices <- dmGetSortedLevelIndices(plotMetricSubspaceParametersList)
    for(sortedLevelIndex in sortedLevelIndices) {
      plotMetricSubspaceParameters <- plotMetricSubspaceParametersList[[sortedLevelIndex]]
      dmMetricSubspacesPoints(plotMetricSubspaceParameters, batchSize, dimension, columnIndices)
    }
    
    if(evaluateDataSourceFileName != 0 && !is.null(plotEvaluateDataSourceParameters)) {
      evaluateLevel <- plotEvaluateDataSourceParameters[[1]]
        
      dmEvaluateDataSourceRead(evaluateDataSourceFileName)
      dmEvaluateDataSourcePoints(evaluateLevel, batchSize, dimension, columnIndices, plotEvaluateDataSourceParameters[[2]], TRUE)
      dmEvaluateDataSourcePoints(evaluateLevel, batchSize, dimension, columnIndices, plotEvaluateDataSourceParameters[[2]], FALSE)
    }
    
    for(index in 1:length(sortedLevelIndices)) {
      lPlotMetricSubspaceParameters <- plotMetricSubspaceParametersList[[sortedLevelIndices[index]]]
      rPlotMetricSubspaceParameters <- NULL
      if(index < length(sortedLevelIndices)) {
        rPlotMetricSubspaceParameters <- plotMetricSubspaceParametersList[[sortedLevelIndices[index + 1]]]
      }
      dmMetricSubspaceLabelPoints(lPlotMetricSubspaceParameters, rPlotMetricSubspaceParameters, batchSize, dimension, columnIndices)
    }
    
  }
  
  dmLegend(sortedLevelIndices,
           plotMetricSubspaceParametersList,
           plotEvaluateDataSourceParameters)
  
  dev.off()
  
  return()
}

#' Create an image file for metric subspaces
#' 
#' Create an image file containing two-dimensional projections of generative data
#' contained in metric subspaces in a data model and optionally an evaluated data source.
#' Plot parameters are passed by a list of generated plot parameters for different levels
#' by dmPlotMetricSubspaceParameters() and by dmPlotEvaluateDataSourceParameters().
#' Data points are drawn in the order generative data contained in metric subspaces by increasing level
#' and evaluated data source.
#'
#' @param plotMetricSubspaceParametersList List of plot parameters for metric subspaces for different levels,
#' see dmPlotMetricSubspaceParameters().
#' @param imageFileName Name of image file
#' @param title Title of image
#' @param columnIndices Vector of two column indices that are used for the two-dimensional projection.
#' Indices refer to indices of active columns of the data source used to create the data model.
#' @param evaluateDataSourceFileName Name of evaluated data source file
#' @param plotEvaluateDataSourceParameters Plot parameters for evaluated data source,
#' see dmPlotEvaluateDataSourceParameters().
#' 
#' @return None
#' @export
#' 
#' @examples
#' \dontrun{
#' dmRead("dm.bin", "gd.bin")
#' dmPlotMetricSubspaces(
#'   list(dmPlotMetricSubspaceParameters(level = 0.7,
#'                                        labels = c("*"),
#'                                        percent = 50,
#'                                        boundary = TRUE,
#'                                        color = "red",
#'                                        backgroundPercent = 0,
#'                                        backgroundColor = "red",
#'                                        backgroundReset = TRUE,
#'                                        plotLabels = TRUE)),
#'   "ms.png",
#'   "Metric Subspaces for the Iris Dataset",
#'   c(3, 4),
#'   "ds.bin",
#'   dmPlotEvaluateDataSourceParameters(0.67))}
dmPlotMetricSubspaces <- function(
  plotMetricSubspaceParametersList = list(),
  imageFileName,
  title,
  columnIndices, 
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
  
  dmPng(plotMetricSubspaceParametersList,
        imageFileName,
        title,
        columnIndices, 
        evaluateDataSourceFileName,
        plotEvaluateDataSourceParameters,
        dimension)
  
  return()
}
