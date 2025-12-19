# Copyright 2021 Werner Mueller
# Released under the GPL (>= 2)

library(tensorflow)
library(Rcpp)

Sys.setenv("PKG_CXXFLAGS"="-std=c++17")
sourceCpp("src/dmInt.cpp")
source("R/keras_zip_lists.R")

utils::globalVariables(c("tape"))

dmTrainSub <- function(dataModelFileName, dataModelRead, numberOfTrainingIterations, numberOfHiddenLayerUnits) {
    cWriteMessageModulo = 100
    cNumberOfBatchesPerIteration <- 10
    cNumberOfInitializationIterations <- 1000

    batchSize <- dmGetBatchSize()
    dataDimension <- dmGetGenerativeDataDimension()
    cEpsilon <- 1.0e-10
    learningRate <- 0.0001

    if(dataModelRead) {
        numberOfHiddenLayerUnits = dmDataModelGetNumberOfHiddenLayerUnits()
    }

    numberOfHiddenLayerUnits <- as.integer(numberOfHiddenLayerUnits)

    discriminatorHiddenLayer1 <- tf$keras$layers$Dense(units = numberOfHiddenLayerUnits, activation = tf$nn$leaky_relu)
    discriminatorHiddenLayer2 <- tf$keras$layers$Dense(units = numberOfHiddenLayerUnits, activation = tf$nn$leaky_relu)
    discriminatorLogits <- tf$keras$layers$Dense(units = 1L)

    discriminatorOptimizer <- tf$keras$optimizers$RMSprop(learning_rate = learningRate, epsilon = cEpsilon)

    checkPoint <- tf$train$Checkpoint(discriminatorHiddenLayer1 = discriminatorHiddenLayer1,
                                      discriminatorHiddenLayer2 = discriminatorHiddenLayer2,
                                      discriminatorLogits = discriminatorLogits,
                                      discriminatorOptimizer = discriminatorOptimizer)

    if(dataModelRead) {
        checkPoint$read(dmGetFileName(dataModelFileName))
    }

    discriminatorNetwork <- function(input) {
        discriminatorHiddenLayer1 <- discriminatorHiddenLayer1(input)
        discriminatorHiddenLayer2 <- discriminatorHiddenLayer2(discriminatorHiddenLayer1)
        logits <- discriminatorLogits(discriminatorHiddenLayer2)
    }

    loss <- function(logitsY, valuesY) {
        r <- tf$reduce_mean(tf$square(logitsY - valuesY))
    }

    trainingCore <- tf_function(function(data, densityValues) {
        with(tf$GradientTape(persistent = TRUE) %as% tape, {
            logits <- discriminatorNetwork(data)
            densityValuesFloat32 <- logits
            densityValuesFloat32 <- tf$cast(densityValues, tf$float32)
            discriminatorLoss <- loss(logits, densityValuesFloat32)
        })

        discriminatorVariables <- append(discriminatorHiddenLayer1$trainable_weights, discriminatorHiddenLayer2$trainable_weights)
        discriminatorVariables <- append(discriminatorVariables, discriminatorLogits$trainable_weights)
        discriminatorGradients <- tape$gradient(discriminatorLoss, discriminatorVariables)
        discriminatorOptimizer$apply_gradients(keras_zip_lists(discriminatorGradients, discriminatorVariables))

        loss <- list()
        loss[[1]] <- discriminatorLoss

        return(loss)
    })

    trainingIteration <- function(iteration, step) {
        loss <- list()
        loss[[1]] = 0
        for(i in 1:cNumberOfBatchesPerIteration) {
            dataRandom <- dmGenerativeDataGetNormalizedDataRandomWithDensities(batchSize)
            data <- array_reshape(dataRandom[1], c(batchSize, dataDimension))
            densityValues <- array_reshape(dataRandom[2], c(batchSize, 1))

            if(step == "Initialize") {
                data <- array(runif(batchSize * dataDimension, 0.0, 1.0), c(batchSize, dataDimension))
            }

            l <- trainingCore(data, densityValues)
            loss[[1]] <- loss[[1]] + l[[1]]
        }

        loss[[1]] <- loss[[1]] / cNumberOfBatchesPerIteration
        return(loss)
    }

    train <- function(dataModelFileName){
        if(!dataModelRead) {
            message("Initialization iteration   Loss")

            for(iteration in 1:cNumberOfInitializationIterations) {
                #loss <- trainingIteration(iteration)
                loss <- trainingIteration(iteration, "Initialize")

                if(iteration %% cWriteMessageModulo == 0) {
                    message(iteration, "   ", format(round(as.numeric(loss[[1]]), 6)))
                }
            }
        }

        message("Training iteration   Loss")

        for(iteration in 1:numberOfTrainingIterations) {
            #loss <- trainingIteration(iteration)
            loss <- trainingIteration(iteration, "Training")

            if(iteration %% cWriteMessageModulo == 0) {
                message(iteration, "   ", format(round(as.numeric(loss[[1]]), 6)))
            }
        }

        if(!is.null(dataModelFileName) && nchar(dataModelFileName) > 0) {
            if(dataModelRead) {
                dmDataModelSetNumberOfTrainingIterations(dmDataModelGetNumberOfTrainingIterations() + numberOfTrainingIterations)
            } else {
                dmCreateGenerativeModel()
                dmDataModelSetNumberOfTrainingIterations(numberOfTrainingIterations)
                dmDataModelSetNumberOfHiddenLayerUnits(numberOfHiddenLayerUnits)
            }

            checkPoint$write(dmGetFileName(dataModelFileName))
            dmWriteWithReadingTrainedModel(dataModelFileName)
        }
    }

    train(dataModelFileName)
}

#' Train a neural network which approximates density values for a data source
#'
#' Read a data source and generative data from files,
#' train a neural network
#' which approximates density values for a data source in iterative training steps,
#' create a data model containing the trained neural network
#' and write it to a file in binary format.
#'
#' @param dataModelFileName Name of data model file
#' @param dataSourceFileName Name of data source file
#' @param generativeDataFileName Name of generative data file
#' @param numberOfIterations Number of iterations.
#' @param numberOfHiddenLayerUnits Number of hidden layer units
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' dmTrain("dm.bin", "ds.bin", "gd.bin", 10000)}
dmTrain <- function(dataModelFileName, dataSourceFileName, generativeDataFileName, numberOfIterations, numberOfHiddenLayerUnits = 512) {
    start <- Sys.time()

    dmDataSourceRead(dataSourceFileName)
    dmGenerativeDataRead(generativeDataFileName)

    dataModelRead <- FALSE
    if(!is.null(dataModelFileName) && nchar(dataModelFileName) > 0) {
        dataModelRead <- dmReadDataModel(dataModelFileName)
    } else {
        stop("No dataModelFileName specified")
    }

    dmTrainSub(dataModelFileName, dataModelRead, numberOfIterations, numberOfHiddenLayerUnits)

    end <- Sys.time()
    message(round(difftime(end, start, units = "secs"), 3), " seconds")
}
