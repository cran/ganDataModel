# Copyright 2021 Werner Mueller
# Released under the GPL (>= 2)

library(tensorflow)
library(Rcpp)
Sys.setenv("PKG_CXXFLAGS"="-std=c++17")
sourceCpp("src/dmInt.cpp")

dmTrainSub <- function(numberOfIterations, dataModelFileName) {
  gdTf <- tf$compat$v1
  gdTf$disable_v2_behavior()

  cNumberOfBatchesPerIteration <- 10  
  cInitIterations <- -100
  lr <- 0.0001
  batchSize <- dmGetBatchSize()
  dataDimension <- dmGetGenerativeDataDimension()

  gdTf$reset_default_graph()
  
  x <- gdTf$placeholder(tf$float32, shape = c(batchSize, dataDimension))
  y <- gdTf$placeholder(tf$float32, shape = c(batchSize, 1L))
  
  discriminator <- function(x, hsize = c(512, 512), reuse=FALSE) {
    with (gdTf$variable_scope('GAN/Discriminator', reuse=reuse), {
      denseLayer1 <- gdTf$layers$dense(inputs = x, units = hsize[1], name = "dl1")
      leakyRelu1 <- gdTf$nn$leaky_relu(denseLayer1)

      denseLayer2 <- gdTf$layers$dense(inputs = leakyRelu1, units = hsize[2], name = "dl2")
      leakyRelu2 <- gdTf$nn$leaky_relu(denseLayer2)

      denseLayer3 <- gdTf$layers$dense(inputs = leakyRelu2, units = 1, name = "dl3")
      leakyRelu3 <- gdTf$nn$leaky_relu(denseLayer3)

      denseLayer4 <- gdTf$layers$dense(inputs = leakyRelu3, units = 1, name = "dl4")
      leakyRelu4 <- gdTf$nn$leaky_relu(denseLayer4)

      logits <- gdTf$layers$dense(inputs = leakyRelu4, units = 1, name = "l")
    })
    list(denseLayer1, denseLayer2, denseLayer3, denseLayer4, logits)
  }
  
  discriminatorLayers <- discriminator(x, reuse = gdTf$AUTO_REUSE)

  denseLayerIdentity <- function(denseLayer) {
    r <- gdTf$identity(denseLayer)
  }

  denseLayer1 <- denseLayerIdentity(discriminatorLayers[[1]])
  denseLayer2 <- denseLayerIdentity(discriminatorLayers[[2]])
  denseLayer3 <- denseLayerIdentity(discriminatorLayers[[3]])
  denseLayer4 <- denseLayerIdentity(discriminatorLayers[[4]])
  logits <- denseLayerIdentity(discriminatorLayers[[5]])

  loss <- function(logitsY, valuesY) {
    r <- gdTf$reduce_mean(gdTf$square(logitsY - valuesY))
  }
  discriminatorLoss <- loss(logits, y)

  vars = gdTf$get_collection(gdTf$GraphKeys$GLOBAL_VARIABLES, scope="GAN/Discriminator")
  trainer <- gdTf$train$RMSPropOptimizer(lr)$minimize(discriminatorLoss, var_list = vars)
  
  init <- gdTf$global_variables_initializer()
  session <- gdTf$Session()
  session$run(init)
  iteration <- 1
  message("Iteration", "   ", "Mean square error")
  
  for(iteration in cInitIterations:numberOfIterations) {
    r <- 1
    for (i in 1:cNumberOfBatchesPerIteration) {
      dataRandom <- dmGenerativeDataGetNormalizedDataRandomWithDensities(batchSize)

      data <- array_reshape(dataRandom[1], c(batchSize, dataDimension))
      densityValues <- array_reshape(dataRandom[2], c(batchSize, 1))

      if(iteration < 1) {
        data <- array_reshape(runif(batchSize * dataDimension, 0.0, 1.0), c(batchSize, dataDimension))
        densityValues <- array_reshape(runif(batchSize, 0.0, 1.0), c(batchSize, 1))
      }

      r <- session$run(list(trainer, discriminatorLoss), feed_dict = dict(x = data, y = densityValues))
    }
    message(iteration, "   ", format(round(r[[2]], 6)))
  }

  saver <- gdTf$train$Saver()
  dm <- dmBuildFileName(dataModelFileName, "")
  saver$save(session, dm)
  session$close()
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
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' dmTrain("dm.bin", "ds.bin", "gd.bin", 10000)}
dmTrain <- function(dataModelFileName, dataSourceFileName, generativeDataFileName, numberOfIterations) {
  start <- Sys.time()
    
  dmDataSourceRead(dataSourceFileName)
  dmGenerativeDataRead(generativeDataFileName)
  dmTrainSub(numberOfIterations, dataModelFileName)
  dmWriteWithReadingTrainedModel(dataModelFileName)
  
  end <- Sys.time()
  message(round(difftime(end, start, units = "secs"), 3), " seconds")
}
