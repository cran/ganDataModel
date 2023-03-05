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
    int buildGraphIterations = 4;
    int minMetricSubspaceSize = 1000;
    
    int nNearestNeighborDistances = 8;
    int nNearestNeighbors = 128;
    
    int nMetricSubspaceNearestNeighborDistances = 8;
    int nMetricSubspaceNearestNeighbors = 8;
  
    const string cMaxSizeExceeded = "Max size of generative data exceeded";
}

vector<int> randomIndicesWithoutReplacement(int size, float percent, int seed = -1) {
    vector<int> indices(size);
    for(int i = 0; i < (int)indices.size(); i++) {
        indices[i] = i;
    }
  
    vector<int> randomIndices;
    random_device rd;
    mt19937 mt(rd());
    if(seed > 0) {
        mt.seed(seed);
    }
  
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
        for(int i = 0; i < (int)randomIndices1.size(); i++) {
            vector<float> numberVector = dmInt::pDataModel->getDataSource().getNormalizedNumberVector(randomIndices1[i]);
            v[0].insert(v[0].end(), numberVector.begin(), numberVector.end());
        }

        for(int i = 0; i < (int)randomIndices1.size(); i++) {
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
        for(int i = 0; i < (int)indexVector.size(); i++) {
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
        for(int i = 0; i < (int)columnVector.size(); i++) {
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
    
        dmInt::pVolumeElementGraph->buildVolumeElementGraphIterative(dmInt::nNearestNeighborDistances, dmInt::nNearestNeighbors, dmInt::buildGraphIterations, true, dmInt::pProgress);
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
        
        dmInt::pDataModel->buildMetricSubspaceRelation();
        
        delete dmInt::pVolumeElementGraph;
        dmInt::pVolumeElementGraph = 0;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
int dmBuildMetricSubspacesSub() {
    try {
        if(dmInt::pGenerativeData == 0) {
            throw string("No generative data");
        }
           
        if(dmInt::pVolumeElementGraph == 0) {
            throw string("No volume element graoh");
        }
    
        int c = dmInt::pVolumeElementGraph->buildMetricSubspaces(dmInt::minMetricSubspaceSize);
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
        for(int i = 0; i < (int)randomIndices1.size(); i++) {
            vector<float> numberVector = ((DataSource*)dmInt::pGenerativeData)->getNormalizedNumberVector(randomIndices1[i]);
            v[0].insert(v[0].end(), numberVector.begin(), numberVector.end());
        }
        for(int i = 0; i < (int)randomIndices1.size(); i++) {
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
std::vector<float> dmGetMetricSubspaceDenormalizedGenerativeData(float level, int metricSubspaceIndex, float percent, bool boundary = false) {
    try {
        if(dmInt::pGenerativeData == 0) {
            throw string("No generative data");
        }
        if(dmInt::pDataModel == 0) {
          throw string("No data model");
        }

        int levelIndex = dmInt::pDataModel->getLevelIndex(level);
        VolumeElementGraph& volumeElementGraph = dmInt::pDataModel->getVolumeElementGraphs()[levelIndex];
        
        vector<int> metricSubspaceGenerativeDataIndices;
        metricSubspaceGenerativeDataIndices = volumeElementGraph.getGenerativeDataVolumeElementIndices(metricSubspaceIndex - 1, boundary);
        vector<int> randomMetricSubspaceGenerativeDataIndices = randomIndicesWithoutReplacement(metricSubspaceGenerativeDataIndices.size(), percent);
        
        vector<float> metricSubspaceData;
        for(int i = 0; i < (int)randomMetricSubspaceGenerativeDataIndices.size(); i++) {
            int index = metricSubspaceGenerativeDataIndices[randomMetricSubspaceGenerativeDataIndices[i]];
            vector<float> denormalizedNumberVector = dmInt::pGenerativeData->getDenormalizedNumberVector(index);
            metricSubspaceData.insert(metricSubspaceData.end(), denormalizedNumberVector.begin(), denormalizedNumberVector.end());
        }
        
        return metricSubspaceData;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
std::vector<int> dmGetMetricSubspaceIndices(float level, std::vector<std::string> labels) {
    try {
        if(dmInt::pGenerativeData == 0) {
            throw string("No generative data");
        }
        if(dmInt::pDataModel == 0) {
            throw string("No data model");
        }
    
        vector<int> metricSubspacesIndices;
        metricSubspacesIndices = dmInt::pDataModel->getMetricSubspaceIndices(level, labels);
        for(int i = 0; i < (int)metricSubspacesIndices.size(); i++) {
            metricSubspacesIndices[i] += 1;
        }
    
        return metricSubspacesIndices;
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
        for(int i = 0; i < (int)positiveAdjacentElements.size(); i++) {
            list.insert(list.end(), positiveAdjacentElements[i].getIndex());
            list.insert(list.end(), positiveAdjacentElements[i].getDistance());
        }
        for(int i = 0; i < (int)negativeAdjacentElements.size(); i++) {
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

//' Get levels for metric subspaces
//'
//' Get levels for metric subspaces in a data model.
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

//' Get metric subspace properties for a level
//'
//' Get properties of metric subspaces in a data model for a level.
//'
//' @param level Level for metric subspaces
//'
//' @return List of list containing label and size of contained generative data for metric subspaces
//' @export
//'
//' @examples
//' \dontrun{
//' dmRead("dm.bin", "gd.bin")
//' dmGetMetricSubspaceProperties(0.73)}
// [[Rcpp::export]]
List dmGetMetricSubspaceProperties(float level) {
    try {
        if(dmInt::pDataModel == 0) {
            throw string("No data model");
        }
    
        List levelMetricSubspaceList;
        vector<MetricSubspaceEntry>& metricSubspaceEntries = dmInt::pDataModel->getMetricSubspaceRelation().getMetricSubspaceEntries();
        for(int i = 0; i < (int)metricSubspaceEntries.size(); i++) {
            if(metricSubspaceEntries[i].getLevel() == level) {
                List levelMetricSubspace;
                levelMetricSubspace.insert(levelMetricSubspace.end(), metricSubspaceEntries[i].getLabel());
                levelMetricSubspace.insert(levelMetricSubspace.end(), metricSubspaceEntries[i].getMetricSubspaceSize());
          
                levelMetricSubspaceList.insert(levelMetricSubspaceList.end(), levelMetricSubspace);
            }
        }
        return levelMetricSubspaceList;    
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
void dmRemoveMetricSubspacesSub(float level) {
    try {
        if(dmInt::pDataModel == 0) {
            throw string("No data model");
        }

        dmInt::pDataModel->removeMetricSubspaces(level);
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
List dmGetMetricSubspacesSub(List dataRecord, float level) {
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
        
        VpGenerativeData<float> vpGenerativeData(*dmInt::pGenerativeData);
        L2Distance<float> l2Distance;
        VpTree<float> vpTree(&vpGenerativeData, &l2Distance, 0);
        vector<VpElement> nearestNeighbors;
        vpTree.linearSearch(normalizedNumberVector, dmInt:: nMetricSubspaceNearestNeighborDistances, dmInt:: nMetricSubspaceNearestNeighbors, nearestNeighbors);
        
        vector<pair<float, string>> levelMetricSubspaces;
        vector<float> levels = dmInt::pDataModel->getLevels();
        for(int i = 0; i < (int)levels.size(); i++) {
            bool found = false;
            for(int j = 0; j < (int)nearestNeighbors.size(); j++) {
                int levelIndex = dmInt::pDataModel->getLevelIndex(levels[i]);
                VolumeElementGraph& volumeElementGraph = dmInt::pDataModel->getVolumeElementGraphs()[levelIndex];
                int volumeElementIndex = volumeElementGraph.getGenerativeDataVolumeElementIndices()[nearestNeighbors[j].getIndex()];
 
                int metricSubspaceElementIndex = volumeElementGraph.getVolumeElements()[volumeElementIndex].getMetricSubspaceElementIndex();
                int metricSubspaceIndex = volumeElementGraph.getMetricSubspaceElements()[metricSubspaceElementIndex].getMetricSubspaceIndex();
 
                string label = dmInt::pDataModel->getMetricSubspaceLabel(levels[i], metricSubspaceIndex);
                
                if(level >= levels[i]) {
                    found = true;
                    levelMetricSubspaces.push_back(make_pair(levels[i], label));
                    break;
                }
            }
            if(!found) {
                levelMetricSubspaces.push_back(make_pair(levels[i], ""));
            }
        }

        List levelMetricSubspaceList;
        for(int i = 0; i < (int)levelMetricSubspaces.size(); i++) {
            List levelMetricSubspace;
            levelMetricSubspace.insert(levelMetricSubspace.end(), levelMetricSubspaces[i].first);
            levelMetricSubspace.insert(levelMetricSubspace.end(), levelMetricSubspaces[i].second);
            
            levelMetricSubspaceList.insert(levelMetricSubspaceList.end(), levelMetricSubspace);
        }
        
        return levelMetricSubspaceList;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
List dmMetricSubspaceLabelPointsSub(float lLevel, float rLevel, float percent, std::vector<int>& columnIndices, const std::vector<std::string>& lLabels) {
    try {
        if(dmInt::pGenerativeData == 0) {
            throw string("No generative data");
        }
        if(dmInt::pDataModel == 0) {
            throw string("No data model");
        }
        
        vector<float> metricSubspacesData;
        vector<string> metricSubspaceLabels;
        vector<int> metricSubspaceIndices = dmInt::pDataModel->getMetricSubspaceIndices(lLevel, lLabels);
        for(int i = 0; i < (int)metricSubspaceIndices.size(); i++) {
            int metricSubspaceIndex = metricSubspaceIndices[i];
            vector<int> generativeDataIndices = dmInt::pDataModel->getMetricSubspaceGenerativeDataIndices(lLevel, rLevel, metricSubspaceIndex);
            
            vector<int> randomIndices = randomIndicesWithoutReplacement(generativeDataIndices.size(), percent, lLevel * 100 + metricSubspaceIndex);
            vector<int> r(randomIndices.size(), -1);
            for(int j = 0; j < (int)randomIndices.size(); j++) {
                r[j] = generativeDataIndices[randomIndices[j]];
            }
            
            VpIndexGenerativeData<float> vpGenerativeData(*dmInt::pGenerativeData, r);
            vector<float> d(dmInt::pGenerativeData->getDimension(), nan(""));
            for(int j = 0; j < (int)columnIndices.size(); j++) {
                d[columnIndices[j] - 1] = 1;
            }
            L2DistanceNanIndexed<float> distance(d);
            VpTree<float> vpTree;
            vpTree.build(&vpGenerativeData, &distance, 0);
        
            int levelIndex = dmInt::pDataModel->getLevelIndex(lLevel);
            VolumeElementGraph& volumeElementGraph = dmInt::pDataModel->getVolumeElementGraphs()[levelIndex];
            
            vector<int> metricSubspaceGenerativeDataBoundaryIndices = volumeElementGraph.getGenerativeDataVolumeElementIndices(metricSubspaceIndex, true);
            VpIndexGenerativeData<float> vpGenerativeDataBoundary(*dmInt::pGenerativeData, metricSubspaceGenerativeDataBoundaryIndices);
            VpTree<float> vpBoundaryTree;
            L2Distance<float> l2Distance;
            vpBoundaryTree.build(&vpGenerativeDataBoundary, &l2Distance, 0);
            
            vector<int> metricSubspaceGenerativeDataIndices = volumeElementGraph.getGenerativeDataVolumeElementIndices(metricSubspaceIndex, false);
            vector<int> randomMetricSubspaceIndices = randomIndicesWithoutReplacement(metricSubspaceGenerativeDataIndices.size(), percent, lLevel * 100 + metricSubspaceIndex);
            float maxDistance = 0;
            int maxIndex = -1;
            for(int j = 0; j <(int)randomMetricSubspaceIndices.size(); j++) {
                vector<float> numberVector = dmInt::pGenerativeData->getNormalizedNumberVector(metricSubspaceGenerativeDataIndices[randomMetricSubspaceIndices[j]]);
                vector<VpElement> nearestNeighbors;
                vpTree.search(numberVector, 1, 1, nearestNeighbors);
                
                vector<VpElement> nearestNeighborsBoundary;
                vpBoundaryTree.search(numberVector, 1, 1, nearestNeighborsBoundary);
                float minDistance = numeric_limits<float>::max();
                if(nearestNeighbors.size() > 0) {
                    if(minDistance > nearestNeighbors[0].getDistance()) {
                        minDistance = nearestNeighbors[0].getDistance();
                    }
                }
                if(nearestNeighborsBoundary.size() > 0) {
                    if(minDistance > nearestNeighborsBoundary[0].getDistance()) {
                        minDistance = nearestNeighborsBoundary[0].getDistance();
                    }
                }
                if(minDistance > maxDistance) {
                    maxDistance = minDistance;
                    maxIndex = metricSubspaceGenerativeDataIndices[randomMetricSubspaceIndices[j]];
                }
            }
            vector<float> denormalizedNumberVector;
            if(maxIndex != -1) {
                denormalizedNumberVector = dmInt::pGenerativeData->getDenormalizedNumberVector(maxIndex);
            }
            metricSubspacesData.insert(metricSubspacesData.end(), denormalizedNumberVector.begin(), denormalizedNumberVector.end());
            
            string label =  dmInt::pDataModel->getMetricSubspaceLabel(lLevel, metricSubspaceIndex);
            metricSubspaceLabels.push_back(label);
        }
        
        List metricSubspaceKeyPoints;
        metricSubspaceKeyPoints.insert(metricSubspaceKeyPoints.end(), metricSubspacesData);
        metricSubspaceKeyPoints.insert(metricSubspaceKeyPoints.end(), metricSubspaceLabels);
        
        return metricSubspaceKeyPoints;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}

// [[Rcpp::export]]
std::vector<int> dmSortLevelIndices(std::vector<float>& levels) {
    try {
        vector<pair<float, int>> levelIndexPairs;
        for(int i = 0; i < (int)levels.size(); i++) {
            levelIndexPairs.push_back(make_pair(levels[i], i + 1));
        }
        sort(levelIndexPairs.begin(), levelIndexPairs.end());
        
        vector<int> sortedLevelIndices;
        for(int i = 0; i < (int)levelIndexPairs.size(); i++) {
            sortedLevelIndices.push_back(levelIndexPairs[i].second);
        }
        return sortedLevelIndices;
    } catch (const string& e) {
        ::Rf_error(e.c_str());
    } catch(...) {
        ::Rf_error("C++ exception (unknown reason)");
    }
}
