\name{ganDataModel-package}
\alias{ganDataModel-package}
\alias{ganDataModel}
\docType{package}
\title{
Build a Metric Subspaces Data Model for a Data Source
}
\description{
Neural networks are applied to create a density value function which approximates density values for a data source. The trained neural network is analyzed for different levels. For each level metric subspaces with density values above a level are determined. The obtained set of metric subspaces and the trained neural network are assembled into a data model. A prerequisite is the definition of a data source, the generation of generative data and the calculation of density values. These tasks are executed using package 'ganGenerativeData' <https://cran.r-project.org/package=ganGenerativeData>.

Properties of built metric subspaces:

1. They contain data with continously varying density values above a level.\cr
2. They have the topological property connected. In topology a space is connected when it cannot be represented as the union of disjoint open subspaces.\cr
3. An inclusion relation is defined by levels: Higher level metric subspcaes are contained in lower level ones. 

The inserted images show two-dimensional projections of generative data contained in metric subspaces with assigned labels for the iris dataset.\cr
\cr
\cr
\cr
\cr
\cr
\if{html}{\figure{dm12.png}}
\if{latex}{\figure{dm12.png}}
\if{html}{\figure{dm34.png}}
\if{latex}{\figure{dm34.png}}
\if{html}{\figure{dm12c.png}}
\if{latex}{\figure{dm12c.png}}
\if{html}{\figure{dm34c.png}}
\if{latex}{\figure{dm34c.png}}
}
\details{
The API includes  main functions \code{dmTrain()} and \code{dmBuildMetricSubspaces()}. \code{dmTrain()} trains a neural network that approximates density values for a data source. \code{dmBuildMetricSubspaces()} analyzes the trained neural network for a level and determines metric subspaces with density values above the level. The API is used as follows:\cr

\strong{1. Prerequisite for building a metric subspaces data model: Create a data source, generate generative data and calculate density values using package \pkg{ganGenerativeData}}\cr

\strong{\code{dsCreateWithDataFrame()}} Create a data source with passed data frame.\cr

\strong{\code{dsDeactivateColumns()}} Deactivate columns of a data source in order to exclude them in generation of generative data. In current version only columns with values of type double or float can be used in generation of generative data. All columns with values of other type have to be deactivated.\cr

\strong{\code{dsWrite()}} Write created data source including settings of active columns to a file in binary format.\cr

\strong{\code{gdGenerate()}} Read a data source from a file, generate generative data for the data source in iterative training steps and write generated data to a file in binary format.\cr

\strong{\code{gdCalculateDensityValues()}} Read generative data from a file, calculate density values and write generative data with assigned density values to original file.\cr

\strong{2. Build a metric subspaces data model}\cr

\strong{\code{dmTrain()}} Read a data source and generative data from files,
train a neural network which approximates density values for a data source in iterative training steps,
create a data model containing the trained neural network and write it to a file in binary format.\cr

\strong{\code{dmBuildMetricSubspaces()}} Read a data model and generative data from files,
analyze the trained neural network in the data model for a level,
determine metric subspaces with density values above the level,
add obtained metric subspaces to the data model and write it to original file.\cr

\strong{\code{dmRemoveMetricSubspaces()}} Remove metric subspaces in a data model for a level.\cr

\strong{\code{dmRead()}} Read a data model and generative data from files.\cr

\strong{\code{dmGetLevels()}} Get levels for metric subspaces in a data model.\cr

\strong{\code{dmGetMetricSubspacesProperties()}} Get metric subspace properties in a data model for a level.\cr

\strong{\code{dmGetContainedInMetricSubspaces()}} Get metric subspaces in a data model in which a data record is contained.\cr

\strong{\code{dmPlotMetricSubspaceParameters()}} Specify plot parameters for metric subspaces for a level.\cr

\strong{\code{dmPlotEvaluateDataSourceParameters()}} Specify plot parameters for evaluated data source.\cr

\strong{\code{dmPlotMetricSubspaces()}} Create an image file containing two-dimensional projections of generative data contained in metric subspaces and evaluated data source.\cr

\strong{\code{dmReset()}} Reset API.\cr

}
\author{
Werner Mueller

Maintainer: Werner Mueller <werner.mueller5@chello.at>
}
\references{
Package 'ganGenerativeData' <https://cran.r-project.org/package=ganGenerativeData>
}
\keyword{ package }
\examples{
# Environment used for execution of examples:

# Operating system: Ubuntu 22.04.1
# Compiler: g++ 11.3.0 (supports C++17 standard)
# R applications: R 4.1.2, RStudio 2022.02.2
# Installed packages: 'Rcpp' 1.0.11, 'tensorflow' 2.11.0,
# 'ganGenerativeData' 1.4.2, 'ganDataModel' 1.1.2

# Package 'tensorflow' provides an interface to machine learning framework
# TensorFlow. To complete the installation function install_tensorflow() has to
# be called.
\dontrun{
library(tensorflow)
install_tensorflow()}

# 1. Prerequisite for building a metric subspaces data model for the iris
# dataset: Create a data source, generate generative data and calculate density
# values for the iris dataset.

# Load library
\dontrun{
library(ganGenerativeData)}

# Create a data source with passed iris data frame.
\dontrun{
dsCreateWithDataFrame(iris)}

# Deactivate the column with index 5 and name Species in order to exclude it in
# generation of generative data.
\dontrun{
dsDeactivateColumns(c(5))}

# Write the data source including settings of active columns to file "ds.bin" in
# binary format.
\dontrun{
dsWrite("ds.bin")}

# Read data source from file "ds.bin",
# generate generative data in iterative training steps (in tests 50000
# iterations and 1024 hidden layer units are used)) and write generated
# generative data to file "gd.bin".
\dontrun{
gdGenerate("gd.bin", "ds.bin", c(1, 2), gdGenerateParameters(2000))}
# Read generative data from file "gd.bin", calculate density values and
# write generative data with density values to original file.
\dontrun{
gdCalculateDensityValues("gd.bin")}

# 2. Build a metric subspaces data model for the iris data set

# Load  library
\dontrun{
library(ganDataModel)}

# Read a data source and generative data from files "ds.bin" and "gd.bin",
# train a neural network which approximates density values for a data source
# in iterative training steps (in tests 250000 iterations are used),
# create a data model containing the trained neural network
# and write it to a file "dm.bin" in binary format.
\dontrun{
dmTrain("dm.bin", "ds.bin", "gd.bin", 10000)}

# Read a data model and generative data from files "dm.bin" and "gd.bin",
# build metric subspaces for level 0.7,
# add obtained metric subspaces to the data model
# and write it to original file.
\dontrun{
dmBuildMetricSubspaces("dm.bin", 0.67, "gd.bin")}

# Read a data model and generative data from files "dm.bin" and "gd,bin".
# Read in data is accessed in function dmPlotMetricSubspaces.
\dontrun{
dmRead("dm.bin", "gd.bin")}

# Create an image showing a two-dimensional projection of generative data
# contained in metric subspaces fpr level 0.67 for column indices 3, 4 and write
# it to file "ms.png"
\dontrun{
dmPlotMetricSubspaces(
  list(dmPlotMetricSubspaceParameters(level = 0.67,
                                       labels = c("*"),
                                       percent = 75,
                                       boundary = TRUE,
                                       color = "red",
                                       backgroundPercent = 0,
                                       backgroundColor = "red",
                                       backgroundReset = TRUE,
                                       plotLabels = TRUE)),
  "msl.png",
  "Metric Subspaces for the Iris Dataset",
  c(3, 4),
  "ds.bin",
  dmPlotEvaluateDataSourceParameters(0.67))}

# Read a data model and generative data from files "dm.bin" and "gd.bin",
# build metric subspaces for level 0.71,
# add obtained metric subspaces to the data model
# and write it to original file.
\dontrun{
dmBuildMetricSubspaces("dm.bin", 0.71, "gd.bin")}

# Read a data model and generative data from files "dm.bin" and "gd,bin".
# Read in data is accessed in function dmPlotMetricSubspaces.
\dontrun{
dmRead("dm.bin", "gd.bin")}

# Create an image showing a two-dimensional projection of generative data
# contained in metric subspaces for levels 0.67, 0.71 for column indices 3, 4
# and write it to file "msls.png"
\dontrun{
dmPlotMetricSubspaces(
  list(dmPlotMetricSubspaceParameters(level = 0.67,
                                       labels = c("*"),
                                       percent = 75,
                                       boundary = TRUE,
                                       color = "red",
                                       backgroundPercent = 0,
                                       backgroundColor = "red",
                                       backgroundReset = TRUE,
                                       plotLabels = TRUE),
       dmPlotMetricSubspaceParameters(level = 0.71,
                                       labels = c("*"),
                                       percent = 75,
                                       boundary = TRUE,
                                       color = "green",
                                       backgroundPercent = 5,
                                       backgroundColor = "red",
                                       backgroundReset = TRUE,
                                       plotLabels = TRUE)),                                       
  "msls.png",
  "Metric Subspaces for the Iris Dataset",
  c(3, 4),
  "ds.bin",
  dmPlotEvaluateDataSourceParameters(0.67))}
}