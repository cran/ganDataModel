// Copyright 2021 Werner Mueller
// Released under the GPL (>= 2)

#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

#include "generativeData.h"
#include "dataModel.h"
#include "normalizeData.h"
#include "volumeElementGraph.h"

namespace dmInt {
    DataSource* pDataSource = 0;
    GenerativeData* pGenerativeData = 0;
    DataModel* pDataModel = 0;
    DataSource* pEvaluateDataSource = 0;
    DataSource* pEvaluateCopyDataSource = 0;
    VolumeElementGraph* pVolumeElementGraph = 0;
    
    Progress* pProgress = 0;
  
    int batchSize = 256;
    int maxSize = batchSize * 50000;
    int buildGraphIterations = 5;
    int volumeElementSubspaceMaxSize = 1000;
    int minSubspaceSize = 1000;
    
    int nNearestNeighborDistances = 4;
    int nNearestNeighbours = 128;
    
    int nSubspaceNearestNeighborDistances = 8;
    int nSubspaceNearestNeighbours = 8;
  
    const string cMaxSizeExceeded = "Max size of generative data exceeded";
}

vector<int> randomIndicesWithoutReplacement(int size, float percent) {
    vector<int> indices(size);
    for(int i = 0; i < indices.size(); i++) {
        indices[i] = i;
    }
  
    vector<int> randomIndices;
    random_device rd;
    mt19937 mt(rd());
  
    int n = round((float)size * percent / 100);
    for(int i = 0; i < n; i++) {
        if(indices.size() > 0) {
            uniform_int_distribution<int> uid(0, indices.size() - 1);
            int r = uid(mt);
            randomIndices.push_back(indices[r]);
            indices[r] = indices[indices.size() - 1];
            indices.resize(indices.size() - 1);
        }
    }
  
    return randomIndices;
}

// [[Rcpp::export]]
void dmProgress(const std::string& step, int size = 0) {
    Function f("message");
    f(step);
    if(size > 0) {
        delete dmInt::pProgress;
        dmInt::pProgress = new Progress(size);
    }
}

// [[Rcpp::export]]
void dmResetSub() {
    try {
        delete dmInt::pDataModel;
        dmInt::pDataModel = 0;

        delete dmInt::pVolumeElementGraph;
        dmInt::pVolumeElementGraph = 0;
        
        delete dmInt::pProgress;
        dmInt::pProgress = 0;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
int dmGetBatchSize() {
    try {
        return dmInt::batchSize;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
int dmGetMaxSize() {
    try {
        return dmInt::maxSize;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
void dmDataSourceRead(const std::string& inFileName) {
    try {
        ifstream is;
        is.open(inFileName.c_str(), std::ios::binary);
        if(!is.is_open()) {
            throw string("File " + inFileName + " could not be opened");
        }
    
        delete dmInt::pDataSource;
        dmInt::pDataSource = new DataSource();
        dmInt::pDataSource->read(is);
        is.close();
    
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
void dmGenerativeDataRead(const std::string& inFileName) {
    try {
        ifstream is;
        is.open(inFileName.c_str(), std::ios::binary);
        if(!is.is_open()) {
            throw string("File " + inFileName + " could not be opened");
        }
    
        delete dmInt::pGenerativeData;
        dmInt::pGenerativeData = new GenerativeData();
        dmInt::pGenerativeData->read(is);
        is.close();
    
        if(dmInt::pGenerativeData->getNormalizedSize() > dmInt::maxSize) {
            throw string(dmInt::cMaxSizeExceeded);
        }
    
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
std::vector<float> dmDataSourceGetNormalizedDataRandom(int rowCount) {
    try {
        if(dmInt::pDataSource == 0) {
            throw string("No data source");
        }
    
        std::vector<float> v = dmInt::pDataSource->getNormalizedDataRandom(rowCount);
        return v;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
std::vector<std::vector<float>> dmDataSourceGetDataRandom(float percent) {
    try {
        if(dmInt::pDataModel == 0) {
            throw string("No data model");
        }
    
        vector<int> randomIndices1 = randomIndicesWithoutReplacement(dmInt::pDataModel->getDataSource().getNormalizedSize(), percent);

        std::vector<vector<float>> v(2);
        for(int i = 0; i < randomIndices1.size(); i++) {
            vector<float> numberVector = dmInt::pDataModel->getDataSource().getNormalizedNumberVector(randomIndices1[i]);
            v[0].insert(v[0].end(), numberVector.begin(), numberVector.end());
        }

        for(int i = 0; i < randomIndices1.size(); i++) {
            vector<float> numberVector = dmInt::pDataModel->getDataSource().getDenormalizedNumberVector(randomIndices1[i]);
            v[1].insert(v[1].end(), numberVector.begin(), numberVector.end());
        }
    
        return v;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
std::vector<float> dmGenerativeDataGetNormalizedDataRandom(int rowCount) {
    try {
        if(dmInt::pGenerativeData == 0) {
            throw string("No generative data");
        }
    
        std::vector<float> v = ((DataSource*)dmInt::pGenerativeData)->getNormalizedDataRandom(rowCount);
        return v;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
std::vector<std::vector<float>> dmGenerativeDataGetNormalizedDataRandomWithDensities(int rowCount) {
    try {
        if(dmInt::pGenerativeData == 0) {
            throw string("No data source");
        }
    
        vector<vector<float>> v = dmInt::pGenerativeData->getNormalizedDataRandomWithDensities(rowCount);
        return v;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
std::vector<float> dmEvaluateCopyDataSourceGetNormalizedData(int row, int rowCount) {
    try {
        if(dmInt::pEvaluateCopyDataSource == 0) {
            throw string("No data source");
        }
    
        std::vector<float> v = dmInt::pEvaluateCopyDataSource->getNormalizedData(row - 1, rowCount);
        return v;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
int dmGetDataSourceDimension() {
    try {
        if(dmInt::pDataModel == 0) {
            throw string("No datamodel");
        }
    
        return dmInt::pDataModel->getDataSource().getDimension();
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
int dmGetGenerativeDataDimension() {
    try {
        if(dmInt::pGenerativeData == 0) {
            throw string("No gnerative data");
        }
    
        return dmInt::pGenerativeData->getDimension();
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
int dmGetNormalizedSize() {
    try {
        if(dmInt::pGenerativeData == 0) {
            throw string("No data source");
        }
    
        return dmInt::pGenerativeData->getNormalizedSize();
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
int dmGetEvaluateCopyDataSourceNormalizedSize() {
    try {
        if(dmInt::pEvaluateCopyDataSource == 0) {
            throw string("No data source");
        }
    
        return dmInt::pEvaluateCopyDataSource->getNormalizedSize();
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
std::string dmBuildFileName(const std::string& fileName, const std::string& extension) {
    try {
        return BuildFileName()(GetFileName()(fileName), extension);
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
void dmWriteWithReadingTrainedModel(const std::string& outFileName) {
    try {
        ofstream os;
        os.open(outFileName.c_str(), std::ios::binary);
        if(!os.is_open()) {
            throw string("File " + outFileName + " could not be opened");
        }
    
        delete dmInt::pDataModel;
        dmInt::pDataModel = new DataModel(*dmInt::pDataSource);
    
        dmInt::pDataModel->writeWithReadingTrainedModel(os, GetFileName()(outFileName));
        os.close();
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
void dmWrite(const std::string& outFileName) {
    try {
        if(dmInt::pDataModel == 0) {
            throw string("No data model");
        }
    
        ofstream os;
        os.open(outFileName.c_str(), std::ios::binary);
        if(!os.is_open()) {
            throw string("File " + outFileName + " could not be opened");
        }
    
        dmInt::pDataModel->write(os, GetFileName()(outFileName));
        os.close();
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
void dmReadDataModel(const std::string& inFileName) {
    try {
        ifstream is;
        is.open(inFileName.c_str(), std::ios::binary);
        if(!is.is_open()) {
            throw string("File " + inFileName + " could not be opened");
        }
    
        delete dmInt::pDataModel;
        dmInt::pDataModel = new DataModel();
    
        dmInt::pDataModel->read(is, GetFileName()(inFileName));
        is.close();
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
void dmEvaluateDataSourceRead(const std::string& inFileName) {
    try {
        ifstream is;
        is.open(inFileName.c_str(), std::ios::binary);
        if(!is.is_open()) {
            throw string("File " + inFileName + " could not be opened");
        }
    
        delete dmInt::pEvaluateDataSource;
        dmInt::pEvaluateDataSource = new DataSource();
        dmInt::pEvaluateDataSource->read(is);
        is.close();
    
        delete dmInt::pEvaluateCopyDataSource;
        dmInt::pEvaluateCopyDataSource = new DataSource(dmInt::pDataModel->getDataSource());
        dmInt::pEvaluateCopyDataSource->addData(*dmInt::pEvaluateDataSource);
    
        NormalizeData normalizeData;
        normalizeData.normalize(*dmInt::pEvaluateCopyDataSource, false);
    
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
std::vector<std::vector<float>> dmGetEvaluateCopyDataSourceNormalizedData(int row, int rowCount) {
    try {
        if(dmInt::pEvaluateCopyDataSource == 0) {
            throw string("No evaluate data source");
        }

        vector<vector<float>> v(2);
        v[0] = dmInt::pEvaluateCopyDataSource->getNormalizedData(row - 1, rowCount);
        v[1] = dmInt::pEvaluateCopyDataSource->getDenormalizedData(row - 1, rowCount);
        
        return v;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
std::vector<float> dmGetEvaluateCopyDataSourceDenormalizedData(int row, int rowCount) {
  try {
    if(dmInt::pEvaluateCopyDataSource == 0) {
      throw string("No evaluate data source");
    }
    
    return dmInt::pEvaluateCopyDataSource->getDenormalizedData(row - 1, rowCount);
  } catch (const string& e) {
    ::Rf_error(e.c_str());
  } catch(...) {
    ::Rf_error("C++ exception (unknown reason)");
  }
}

// [[Rcpp::export]]
std::vector<std::wstring> dmGetNumberVectorIndexNames(std::vector<int>& numberVectorIndices) {
    try {
        if(dmInt::pGenerativeData == 0) {
            throw string("No generative data");
        }
    
        vector<int> indexVector = numberVectorIndices;
        for(int i = 0; i < indexVector.size(); i++) {
            indexVector[i] -= 1;
        }
        vector<wstring> numberVectorIndexNames = dmInt::pGenerativeData->getNumbeVectorIndexNames(indexVector);
        return numberVectorIndexNames;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
std::string dmGetValue(float level) {
    try {
        return GetValue()(level);
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
std::vector<float> dmGenerativeDataGetNormalizedData(int row, int rowCount) {
    try {
        if(dmInt::pGenerativeData == 0) {
            throw string("No generative data");
        }
        std::vector<float> v = ((DataSource*)dmInt::pGenerativeData)->getNormalizedData(row - 1, rowCount);
        return v;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
List dmGetRow(int index) {
    try {
        if(dmInt::pGenerativeData == 0) {
            throw string("No generative data");
        }
    
        List list;
        vector<Column*> const & columnVector = dmInt::pGenerativeData->getColumnVector();
        for(int i = 0; i < columnVector.size(); i++) {
            Column::COLUMN_TYPE type = columnVector[i]->getColumnType();
            if(type == Column::NUMERICAL) {
                vector<float> numberVector = columnVector[i]->getDenormalizedNumberVector(index - 1);
                float value = numberVector[0];
                list.insert(list.end(), value);
            } else {
                throw string(cInvalidColumnType);
            }
        }
        return list;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
void dmReadVolumeElementGraph(const std::string& inFileName) {
    try {
        ifstream is;
        is.open(inFileName.c_str(), ios::binary);
        if(!is.is_open()) {
            throw string("File " + inFileName + " could not be opened");
        }
    
        delete dmInt::pVolumeElementGraph;
        dmInt::pVolumeElementGraph = new VolumeElementGraph();
        dmInt::pVolumeElementGraph->read(is);
        is.close();
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
void dmWriteVolumeElementGraph(const std::string& outFileName) {
    try {
        if(dmInt::pVolumeElementGraph == 0) {
            throw string("No volume element graoh");
        }
    
        ofstream os;
        os.open(outFileName.c_str(), ios::binary);
        if(!os.is_open()) {
            throw string("File " + outFileName + " could not be opened");
        }
  
        dmInt::pVolumeElementGraph->write(os);
        os.close();
    
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
void dmAddVolumeElementsSub(std::vector<float>& volumeElementValues, std::vector<int>& dimensions, int indexBegin, float level = 0.0) {
    try {
        if(dmInt::pVolumeElementGraph == 0) {
            dmInt::pVolumeElementGraph = new VolumeElementGraph(level);
        }
    
        dmInt::pVolumeElementGraph->addVolumeElements(volumeElementValues, dimensions, indexBegin - 1, level, dmInt::pProgress);
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
void dmBuildVolumeElements() {
    try {
        if(dmInt::pGenerativeData == 0) {
            throw string("No generative data");
        }
        if(dmInt::pVolumeElementGraph == 0) {
            throw string("No volume element graoh");
        }
    
        dmInt::pVolumeElementGraph->buildVolumeElements();
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
void dmBuildVolumeElementTree() {
    try {
        if(dmInt::pGenerativeData == 0) {
          throw string("No generative data");
        }
        if(dmInt::pVolumeElementGraph == 0) {
            throw string("No volume element graoh");
        }
    
        dmInt::pVolumeElementGraph->buildVolumeElementTree(dmInt::pProgress);
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
void dmBuildVolumeElementGraph() {
    try {
        if(dmInt::pGenerativeData == 0) {
            throw string("No generative data");
        }
        if(dmInt::pVolumeElementGraph == 0) {
            throw string("No volume element graoh");
        }
        if(!dmInt::pVolumeElementGraph->isVolumeElementTreeBuilt()) {
            throw string("No volume element tree");
        }
    
        dmInt::pVolumeElementGraph->buildVolumeElementGraphIterative(dmInt::nNearestNeighborDistances, dmInt::nNearestNeighbours, dmInt::buildGraphIterations, true, dmInt::pProgress);
        dmInt::pVolumeElementGraph->buildVolumeElementGraphElementSubspace(dmInt::nNearestNeighbours, dmInt::nNearestNeighbours, dmInt::volumeElementSubspaceMaxSize, false);
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
void dmAddVolumeElementGraph() {
    try {
        if(dmInt::pGenerativeData == 0) {
            throw string("No generative data");
        }
        if(dmInt::pVolumeElementGraph == 0) {
            throw string("No volume element graoh");
        }
        if(dmInt::pDataModel == 0) {
            throw string("No data model");
        }
    
        int levelIndex = -1;
        for(int i = 0; i < (int)dmInt::pDataModel->getVolumeElementGraphs().size(); i++) {
            if(dmInt::pDataModel->getVolumeElementGraphs()[i].getLevel() == dmInt::pVolumeElementGraph->getLevel()) {
                levelIndex = i;
                break;
            }
        }
        if(levelIndex == -1 ) {
            dmInt::pDataModel->getVolumeElementGraphs().push_back(VolumeElementGraph(*dmInt::pVolumeElementGraph));
        } else {
            dmInt::pDataModel->getVolumeElementGraphs()[levelIndex] = *dmInt::pVolumeElementGraph;          
        }
        
        delete dmInt::pVolumeElementGraph;
        dmInt::pVolumeElementGraph = 0;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
int dmBuildSubspacesSub() {
    try {
        if(dmInt::pGenerativeData == 0) {
            throw string("No generative data");
        }
           
        if(dmInt::pVolumeElementGraph == 0) {
            throw string("No volume element graoh");
        }
    
        int c = dmInt::pVolumeElementGraph->buildSubspaces(dmInt::minSubspaceSize);
        return c;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
std::vector<std::vector<float>> dmGetGenerativeDataRandom(float percent) {
    try {
        if(dmInt::pGenerativeData == 0) {
            throw string("No generative data");
        }

        vector<int> randomIndices1 = randomIndicesWithoutReplacement(dmInt::pGenerativeData->getNormalizedSize(), percent);

        std::vector<vector<float>> v(2);
        for(int i = 0; i < randomIndices1.size(); i++) {
            vector<float> numberVector = ((DataSource*)dmInt::pGenerativeData)->getNormalizedNumberVector(randomIndices1[i]);
            v[0].insert(v[0].end(), numberVector.begin(), numberVector.end());
        }
        for(int i = 0; i < randomIndices1.size(); i++) {
            vector<float> numberVector = ((DataSource*)dmInt::pGenerativeData)->getDenormalizedNumberVector(randomIndices1[i]);
            v[1].insert(v[1].end(), numberVector.begin(), numberVector.end());
        }
        return v;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
std::vector<float> dmGetSubspaceDenormalizedGenerativeData(float level, int subspace, float percent, bool boundary = false) {
    try {
        if(dmInt::pGenerativeData == 0) {
            throw string("No generative data");
        }
        if(dmInt::pDataModel == 0) {
          throw string("No data model");
        }

        int levelIndex = dmInt::pDataModel->getLevelIndex(level);
        VolumeElementGraph& volumeElementGraph = dmInt::pDataModel->getVolumeElementGraphs()[levelIndex];
        
        int positiveSubspaceIndex = subspace - 1;
        if(positiveSubspaceIndex < 0 || positiveSubspaceIndex >= (int)volumeElementGraph.getPositiveVolumeElementSubspaceIndices().size()) {
            throw string(cInvalidIndex);
        }
        int subspaceIndex = volumeElementGraph.getPositiveVolumeElementSubspaceIndices()[positiveSubspaceIndex];
        
        vector<int> elementSubspaceGenerativeDataIndices;
        vector<int>& elementSubspaceIndices = volumeElementGraph.getVolumeElementSubspaces()[subspaceIndex].getElementSubspaceIndices();
      
        for(int i = 0; i < (int)elementSubspaceIndices.size(); i++) {
            int elementSubspaceIndex = elementSubspaceIndices[i];
            for(int j = 0; j < (int)volumeElementGraph.getVolumeElementElementSubspaces()[elementSubspaceIndex].getVolumeElementIndices().size(); j++) {
                int k = volumeElementGraph.getVolumeElementElementSubspaces()[elementSubspaceIndex].getVolumeElementIndices()[j]; 
                vector<int> vp = volumeElementGraph.getVolumeElements()[k].getGenerativeDataIndices();
                if(!boundary || (boundary && volumeElementGraph.getVolumeElements()[k].isBoundaryElement())) {
                    elementSubspaceGenerativeDataIndices.insert(elementSubspaceGenerativeDataIndices.end(), vp.begin(), vp.end());
                }
            }
        }
        vector<int> generativeDataIndices = randomIndicesWithoutReplacement(elementSubspaceGenerativeDataIndices.size(), percent);
        
        vector<float> subspaceData;
        for(int i = 0; i < (int)generativeDataIndices.size(); i++) {
          int index = elementSubspaceGenerativeDataIndices[generativeDataIndices[i]];
          vector<float> denormalizedNumberVector = dmInt::pGenerativeData->getDenormalizedNumberVector(index);
          subspaceData.insert(subspaceData.end(), denormalizedNumberVector.begin(), denormalizedNumberVector.end());
        }
        
        return subspaceData;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
List dmGetAdjacentVolumeElementIndices(int index) {
    try {
        if(dmInt::pGenerativeData == 0) {
            throw string("No generative data");
        }
        if(dmInt::pVolumeElementGraph == 0) {
            throw string("No volume element graoh");
        }
    
        vector<VpElement> positiveAdjacentElements = dmInt::pVolumeElementGraph->getVolumeElements()[index].getPositiveAdjacentVolumeElements();
        vector<VpElement> negativeAdjacentElements = dmInt::pVolumeElementGraph->getVolumeElements()[index].getNegativeAdjacentVolumeElements();
    
        List list;
        for(int i = 0; i < positiveAdjacentElements.size(); i++) {
            list.insert(list.end(), positiveAdjacentElements[i].getIndex());
            list.insert(list.end(), positiveAdjacentElements[i].getDistance());
        }
        for(int i = 0; i < negativeAdjacentElements.size(); i++) {
            list.insert(list.end(), negativeAdjacentElements[i].getIndex());
            list.insert(list.end(), negativeAdjacentElements[i].getDistance());
        }
    
        return list;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
float dmGetMax(int i) {
    try {
        if(dmInt::pGenerativeData == 0) {
            throw string("No generative data");
        }
    
        if(i - 1 < 0 || i - 1 > dmInt::pGenerativeData->getDimension() - 1) {
            throw string(cInvalidColumnIndex);
        }
        int j = dmInt::pGenerativeData->getColumnIndex(i - 1);
        vector<Column*> const& columnVector = dmInt::pGenerativeData->getColumnVector();
    
        float max = 0;
        Column::COLUMN_TYPE type = columnVector[j]->getColumnType();
        if(type == Column::NUMERICAL) {
            NumberColumn* pNumberColumn = dynamic_cast<NumberColumn*>(columnVector[j]);
            max = pNumberColumn->getMax();
        } else {
            throw string(cInvalidColumnType);
        }
        return max;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
float dmGetMin(int i) {
    try {
        if(dmInt::pGenerativeData == 0) {
            throw string("No generative data");
        }
    
        if(i - 1 < 0 || i - 1 > dmInt::pGenerativeData->getDimension() - 1) {
            throw string(cInvalidColumnIndex);
        }
        int j = dmInt::pGenerativeData->getColumnIndex(i - 1);
        vector<Column*> const& columnVector = dmInt::pGenerativeData->getColumnVector();
    
        float min = 0;
        Column::COLUMN_TYPE type = columnVector[j]->getColumnType();
        if(type == Column::NUMERICAL) {
            NumberColumn* pNumberColumn = dynamic_cast<NumberColumn*>(columnVector[j]);
            min = pNumberColumn->getMin();
        } else {
            throw string(cInvalidColumnType);
        }
        return min;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

//' Get levels for subspaces
//'
//' Get levels for subspaces in a data model.
//'
//' @return Vector of levels
//' @export
//'
//' @examples
//' \dontrun{
//' dmRead("dm.bin", "gd.bin")
//' dmGetLevels()}
// [[Rcpp::export]]
std::vector<float> dmGetLevels() {
    try {
        if(dmInt::pDataModel == 0) {
            throw string("No data model");
        }

        return dmInt::pDataModel->getLevels();
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

//' Get number of subspaces for a level
//'
//' Get number of subspaces in a data model for a level.
//'
//' @param level Level
//'
//' @return Number of subspaces
//' @export
//'
//' @examples
//' \dontrun{
//' dmRead("dm.bin", "gd.bin")
//' dmGetNumberOfSubspaces(0.73)}
// [[Rcpp::export]]
int dmGetNumberOfSubspaces(float level) {
    try {
        if(dmInt::pDataModel == 0) {
            throw string("No data model");
        }
    
        return dmInt::pDataModel->getNumberOfSubspaces(level) ;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
void dmRemoveSubspacesSub(float level) {
    try {
        if(dmInt::pDataModel == 0) {
            throw string("No data model");
        }
    
        for(int i = 0; i < (int)dmInt::pDataModel->getVolumeElementGraphs().size(); i++) {
            if(dmInt::pDataModel->getVolumeElementGraphs()[i].getLevel() == level) {
                dmInt::pDataModel->getVolumeElementGraphs()[i] = dmInt::pDataModel->getVolumeElementGraphs()[dmInt::pDataModel->getVolumeElementGraphs().size() - 1];
                dmInt::pDataModel->getVolumeElementGraphs().pop_back();
                break;
            }
        }
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
std::vector<float> dmNormalizedDataRecord(List dataRecord) {
    try {
      
        if(dmInt::pDataModel == 0) {
            throw string("No data model");
        }
    
        vector<float> numberVector;
        for(List::iterator iterator = dataRecord.begin(); iterator != dataRecord.end(); ++iterator) {
            float number = (float)as<double>(*iterator);
            numberVector.push_back(number);
        }
        
        NormalizeData normalizeData;
        vector<float> normalizedNumberVector = normalizeData.getNormalizedNumberVector(dmInt::pDataModel->getDataSource() , numberVector);
        
        return normalizedNumberVector;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
List dmGetSubspacesSub(List dataRecord, float level) {
    try {
        if(dmInt::pGenerativeData == 0) {
            throw string("No generative data");
        }
        if(dmInt::pDataModel == 0) {
            throw string("No data model");
        }
 
        vector<float> numberVector;
        for(List::iterator iterator = dataRecord.begin(); iterator != dataRecord.end(); ++iterator) {
            float number = (float)as<double>(*iterator);
            numberVector.push_back(number);
        }
 
        NormalizeData normalizeData;
        vector<float> normalizedNumberVector = normalizeData.getNormalizedNumberVector(dmInt::pDataModel->getDataSource() , numberVector);
 
        VpGenerativeData vpGenerativeData(*dmInt::pGenerativeData);
        L2Distance l2Distance;
        VpTree vpTree(&vpGenerativeData, &l2Distance, 0);
        vector<VpElement> nearestNeighbours;
        vpTree.linearSearch(normalizedNumberVector, dmInt:: nSubspaceNearestNeighborDistances, dmInt:: nSubspaceNearestNeighbours, nearestNeighbours);
 
        vector<pair<float, int>> levelSubspaces;
 
        Function f("message");
 
        vector<float> levels = dmInt::pDataModel->getLevels();
        for(int i = 0; i < (int)levels.size(); i++) {
            bool found = false;
            for(int j = 0; j < nearestNeighbours.size(); j++) {
                int levelIndex = dmInt::pDataModel->getLevelIndex(levels[i]);
                VolumeElementGraph& volumeElementGraph = dmInt::pDataModel->getVolumeElementGraphs()[levelIndex];
                int volumeElementIndex = volumeElementGraph.getGenerativeDataVolumeElementIndices()[nearestNeighbours[j].getIndex()];
 
                int elementSubspaceIndex = volumeElementGraph.getVolumeElements()[volumeElementIndex].getElementSubspaceIndex();
                int subspaceIndex = volumeElementGraph.getVolumeElementElementSubspaces()[elementSubspaceIndex].getSubspaceIndex();
 
                vector<int>::iterator positiveVolumeElementSubspaceIndicesIter;
                positiveVolumeElementSubspaceIndicesIter = lower_bound(volumeElementGraph.getPositiveVolumeElementSubspaceIndices().begin(), volumeElementGraph.getPositiveVolumeElementSubspaceIndices().end(), subspaceIndex);
                if(positiveVolumeElementSubspaceIndicesIter != volumeElementGraph.getPositiveVolumeElementSubspaceIndices().end() &&
                    *positiveVolumeElementSubspaceIndicesIter == subspaceIndex &&
                    level >= levels[i]) {
                    int index = positiveVolumeElementSubspaceIndicesIter - volumeElementGraph.getPositiveVolumeElementSubspaceIndices().begin() + 1;
                    levelSubspaces.push_back(make_pair(levels[i], index));
                    found = true;
                    break;
                }
            }
            if(!found) {
                levelSubspaces.push_back(make_pair(levels[i], -1));
            }
        }
        
        List levelSubspaceList;
        for(int i = 0; i < (int)levelSubspaces.size(); i++) {
            List levelSubspace;
            levelSubspace.insert(levelSubspace.end(), levelSubspaces[i].first);
            levelSubspace.insert(levelSubspace.end(), levelSubspaces[i].second);
            
            levelSubspaceList.insert(levelSubspaceList.end(), levelSubspace);
        }
        
        return levelSubspaceList;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}
