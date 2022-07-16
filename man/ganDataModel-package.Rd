\name{ganDataModel-package}
\alias{ganDataModel-package}
\alias{ganDataModel}
\docType{package}
\title{
Create a Hierarchical, Categorical Data Model for a Data Source
}
\description{
Neural networks are applied to create a density value function which approximates density values for a data source. The trained neural network is analysed for different levels. For each level subspaces with density values above a level are determined. The obtained set of subspaces categorizes the data source hierarchically. A prerequisite is the definition of a data source, the generation of generative data and the calculation of density values. These tasks are executed using package 'ganGenerativeData' <https://cran.r-project.org/package=ganGenerativeData>.

The inserted images show two-dimensional projections of generative data for the iris dataset for selected subspaces. Generative data assigned to subspaces is displayed in green colour. Subspace 1 for lower level 0.67 contains subspaces 1, 3 and 4 for higher level 0.735.\cr
\cr
\cr
\cr
\if{html}{\figure{dm34dl67s1.png}}
\if{latex}{\figure{dm34dl67s1.png}}
\if{html}{\figure{dm12dl67s1.png}}
\if{latex}{\figure{dm12dl67s1.png}}
\if{html}{\figure{dm34dl735s1.png}}
\if{latex}{\figure{dm34dl735s1.png}}
\if{html}{\figure{dm12dl735s1.png}}
\if{latex}{\figure{dm12dl735s1.png}}
\if{html}{\figure{dm34dl735s3.png}}
\if{latex}{\figure{dm34dl735s3.png}}
\if{html}{\figure{dm12dl735s3.png}}
\if{latex}{\figure{dm12dl735s3.png}}
\if{html}{\figure{dm34dl735s4.png}}
\if{latex}{\figure{dm34dl735s4.png}}
\if{html}{\figure{dm12dl735s4.png}}
\if{latex}{\figure{dm12dl735s4.png}}
}
\details{
The API includes  main functions \code{dmTrain()} and \code{dmBuildSubspaces()}. \code{dmTrain()} trains a neural network that approximates density values for a data source. \code{dmBuildSubspaces()} analyses the trained neural network for a level and determines subspaces with density values above the level. The API is used as follows:\cr

\strong{1. Prerequisite for creating a hierarchical, categorical data model: Create a data source, generate generative data and calculate density values using package \pkg{ganGenerativeData}}\cr

\strong{\code{dsCreateWithDataFrame()}} Create a data source with passed data frame.\cr

\strong{\code{dsDeactivateColumns()}} Deactivate columns of a data source in order to exclude them in generation of generative data. In current version only columns with values of type double or float can be used in generation of generative data. All columns with values of other type have to be deactivated.\cr

\strong{\code{dsWrite()}} Write created data source including settings of active columns to a file in binary format.\cr

\strong{\code{gdGenerate()}} Read a data source from a file, generate generative data for the data source in iterative training steps and write generated data to a file in binary format.\cr

\strong{\code{gdCalculateDensityValues()}} Read generative data from a file, calculate density values and write generative data with assigned density values to original file.\cr

\strong{2. Create a hierarchical, categorical data model}\cr

\strong{\code{dmTrain()}} Read a data source and generative data from files,
train a neural network which approximates density values for a data source in iterative training steps,
create a data model with the trained neural network and write it to a file in binary format.\cr

\strong{\code{dmBuildSubspaces()}} Read a data model and generative data from files,
analyse the trained neural network in the data model for a level, determine subspaces with density values above the level,
add obtained subspaces with assigned level to the data model and write it to original file.\cr

\strong{\code{dmRemoveSubspaces()}} Remove subspaces from a data model for a level.\cr

\strong{\code{dmRead()}} Read a data model and generative data from files.\cr

\strong{\code{dmGetLevels()}} Get levels for subspaces in a data model.\cr

\strong{\code{dmGetNumberOfSubspaces()}} Get number of subspaces in a data model for a level.\cr

\strong{\code{dmGetAssignedSubspaces()}} Get subspaces in a data model to which a data record is assigned\cr

\strong{\code{dmPlotGenerativeDataParameters()}} Specify plot parameters for generative data.\cr

\strong{\code{dmPlotSubspaceParameters()}} Soecify plot parameters for subspaces.\cr

\strong{\code{dmPlotEvaluateDataSourceParameters()}} Specify plot parameters for a data source.\cr

\strong{\code{dmPlotSubspaces()}} Create an image file containing two-dimensional projections of generative data, generative data assigned to subspaces and optionally an evaluated data source.\cr

\strong{\code{dmEvaluateDataRecord()}} Calculate density value for a data record by evaluating trained neural network in a data model.\cr

\strong{\code{dmPlotEvaluate()}} Create an image file containing two-dimensional projections of generative data and an evaluated data source.\cr

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
# Packages ganGenerativeData and ganDataModel import package tensorflow
# which provides an interface to machine learning framework TensorFlow.
# It is used for training of neural networks and must be installed by the user.
\dontrun{
library(tensorflow)
install_tensorflow()}

# 1. Prerequisite for creating a hierarchical, categorical data model for the iris dataset:
# Create a data source, generate generative data and calculate density values for the iris dataset.

# Load library
\dontrun{
library(ganGenerativeData)}

# Create a data source with passed iris data frame.
\dontrun{
dsCreateWithDataFrame(iris)}

# Deactivate the column with name Species and index 5 in order to exclude it in
# generation of generative data.
\dontrun{
dsDeactivateColumns(c(5))}

# Write the data source including settings of active columns to file "ds.bin" in binary format.
\dontrun{
dsWrite("ds.bin")}

# Read data source from file "ds.bin",
# generate generative data in iterative training steps (in practise 50000 iterations are used))
# and write generated generative data to file "gd.bin".
\dontrun{
gdGenerate("ds.bin", "gd.bin", 2500, 0.95, c(1, 2))}

# Read generative data from file "gd.bin", calculate density values and
# write generative data with density values to original file.
\dontrun{
gdCalculateDensityValues("gd.bin")}

# 2. Create a hierarchical, categorical data model for the iris data set

# Load  library
\dontrun{
library(ganDataModel)}

# Read a data source and generative data from files "ds.bin" and "gd.bin",
# train a neural network which approximates density values for a data source
# in iterative training steps (in practise 250000 iterations are used),
# create a data model with trained neural network
# and write it to a file "dm.bin" in binary format.
\dontrun{
dmTrain("dm.bin", "ds.bin", "gd.bin", 10000)}

# Read a data model and generative data from files "dm.bin" and "gd.bin",
# build subspaces for level 0.73,
# add obtained subspaces with assigned level to data model
# and write it to original file.
\dontrun{
dmBuildSubspaces("dm.bin", 0.73, "gd.bin")}

# Read a data model and generative data from files "dm.bin" and "gd,bin".
# Read in data is accessed in function dmPlotSubspaces.
\dontrun{
dmRead("dm.bin", "gd.bin")}

# Create an image showing a two-dimensional projection of generative data
# for column indices 3, 4 for subspace 1 and write it to file "s1d34.png"
\dontrun{
dmPlotSubspaces(0.73,
  c(1),
  "s1d34.png",
  "Subspace of Hierarchical, Categorical Data Model for the Iris Dataset",
  c(3, 4),
  dmPlotGenerativeDataParameters(10),
  dmPlotSubspaceParameters(100),
  "ds.bin",
  dmPlotEvaluateDataSourceParameters()
)}

# Create an image showing a two-dimensional projection of generative data
# for column indices 3, 4 for subspace 2 and write it to file "s2d34.png"
\dontrun{
dmPlotSubspaces(0.73,
  c(2),
  "s2d34.png",
  "Subspace of Hierarchical, Categorical Data Model for the Iris Dataset",
  c(3, 4),
  dmPlotGenerativeDataParameters(10),
  dmPlotSubspaceParameters(100),
  "ds.bin",
  dmPlotEvaluateDataSourceParameters()
)}
}
