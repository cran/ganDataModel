// Copyright 2021 Werner Mueller
// Released under the GPL (>= 2)

#ifndef DATA_MODEL
#define DATA_MODEL

#include <algorithm>
#include <fstream>
#include <string>
#include <vector>
#include <iostream>
#include <iterator>
#include <sstream>
#include "inOut.h"
#include "dataSource.h"
#include "volumeElementGraph.h"

using namespace std;

const string csSeparator = "_";
const string cDataFileExtension = "data-00000-of-00001";
const string cIndexFileExtension = "index";
const string cMetaFileExtension = "meta";
const string cInvalidLevel = "Invalid level";

const string cDataModelTypeId = "2da979bc-77df-4e9e-9fb2-7916e02a001c";

class TrainedModel {
public:
    TrainedModel(): _separator(csSeparator), _period(cPeriod), _dataFileExtension(cDataFileExtension), _indexFileExtension(cIndexFileExtension), _metaFileExtension(cMetaFileExtension) {
        ;
    }
    void readVectors(const string& modelName) {
        readVector(BuildFileName()(modelName, _dataFileExtension), _dataVector);
        readVector(BuildFileName()(modelName, _indexFileExtension), _indexVector);
        readVector(BuildFileName()(modelName, _metaFileExtension), _metaVector);
    }
    void writeVectors(const string& modelName) {
        writeVector(BuildFileName()(modelName, _dataFileExtension), _dataVector);
        writeVector(BuildFileName()(modelName, _indexFileExtension), _indexVector);
        writeVector(BuildFileName()(modelName, _metaFileExtension), _metaVector);
    }
    
    void readVector(const string& inFileName, vector<unsigned char>& dataVector) {
        ifstream inFile;
        inFile.open(inFileName.c_str(), ios::binary | ios::ate);
        if(!inFile.is_open()) {
            throw string("File could not be opened");
        }
        
        streamsize size = inFile.tellg();
        inFile.seekg(0, ios::beg);
        dataVector.resize(size, 0);
        inFile.read((char *)dataVector.data(), size);
        inFile.close();
    }
    void writeVector(const string& outFileName, vector<unsigned char>& dataVector) {
        ofstream outFile;
        outFile.open(outFileName.c_str(), ios::binary);
        if(!outFile.is_open()) {
            throw string("File could not be opened");
        }
        
        streamsize size = dataVector.size();
        outFile.write((char *)dataVector.data(), size);
        outFile.close();
    }
    void write(ofstream& os) {
        InOut::Write(os, _dataVector);
        InOut::Write(os, _indexVector);
        InOut::Write(os, _metaVector);
    }
    void read(ifstream& is) {
        InOut::Read(is, _dataVector);
        InOut::Read(is, _indexVector);
        InOut::Read(is, _metaVector);
    }
    
private:
    string _dataFileName;
    string _indexFileNmae;
    string _metaFileName;
    
    vector<unsigned char> _dataVector;
    vector<unsigned char> _indexVector;
    vector<unsigned char> _metaVector;
    
    string _separator;
    string _period;
    string _dataFileExtension;
    string _indexFileExtension;
    string _metaFileExtension;
};

class DataModel {
public:
    DataModel(): _typeId(cDataModelTypeId), _version(1) {
    }
    DataModel(DataSource& dataSource): _typeId(cDataModelTypeId), _version(1), _dataSource(dataSource) {
    }
    void writeWithReadingTrainedModel(ofstream& os, const string& modelName, int version = 1) {
        _trainedModel.readVectors(modelName);
        
        write(os, modelName, version);
    }
    void write(ofstream& os, const string& modelName, int version = 1) {
        InOut::Write(os, _typeId);
        InOut::Write(os, version);
        
        _dataSource.write(os);
        
        _trainedModel.write(os);
        
        int size = _volumeElementGraphs.size();
        InOut::Write(os, size);
        for(int i = 0; i < (int)_volumeElementGraphs.size(); i++) {
            _volumeElementGraphs[i].write(os);
        }
    }
    void read(ifstream& is, const string& modelName) {
        InOut::Read(is, _typeId);
        if(_typeId != cDataModelTypeId) {
            throw string(cInvalidTypeId);
        }
        InOut::Read(is, _version);
        
        _dataSource.read(is);
        
        _trainedModel.read(is);
        
        _trainedModel.writeVectors(modelName);
        
        int size = 0;
        InOut::Read(is, size);
        _volumeElementGraphs.resize(size);
        for(int i = 0; i < (int)_volumeElementGraphs.size(); i++) {
            _volumeElementGraphs[i].read(is);
        }
    }
    DataSource& getDataSource() {
        return _dataSource;
    }
    TrainedModel& getTrainedModel() {
        return _trainedModel;
    }
    vector<VolumeElementGraph>& getVolumeElementGraphs() {
        return _volumeElementGraphs;
    }
    
    vector<float> getLevels() {
        vector<float> levels;
        for(int i = 0; i < (int)_volumeElementGraphs.size(); i++) {
            levels.push_back(_volumeElementGraphs[i].getLevel());
        }
    
        sort(levels.begin(), levels.end());
        return levels;
    }
    int getLevelIndex(float level) {
        int levelIndex = -1;
        for(int i = 0; i < (int)_volumeElementGraphs.size(); i++) {
            if(_volumeElementGraphs[i].getLevel() == level) {
                levelIndex = i;
                break;
            }
        }
        if(levelIndex == -1 ) {
            throw string(cInvalidLevel);
        }
        return levelIndex;
    }
    int getNumberOfSubspaces(float level) {
        int levelIndex = -1;
        for(int i = 0; i < (int)_volumeElementGraphs.size(); i++) {
            if(_volumeElementGraphs[i].getLevel() == level) {
                levelIndex = i;
                break;
            }
        }
        if(levelIndex == -1 ) {
            throw string(cInvalidLevel);
        }
        
        int numberOfSubspaces = _volumeElementGraphs[levelIndex].getPositiveVolumeElementSubspaceIndices().size();
        return numberOfSubspaces;
    }
    
    int dmGetPositiveVolumeElementSubspaceIndex(float level, VolumeElementConfiguration& volumeElementConfiguration) {
        int subSpaceIndex = -1;
        int levelIndex = getLevelIndex(level);
        VolumeElementGraph& volumeElementGraph = getVolumeElementGraphs()[levelIndex];
        VolumeElement volumeElement(volumeElementConfiguration);
            
        vector<VolumeElement>::iterator volumeElementsIter;
        volumeElementsIter = lower_bound(volumeElementGraph.getVolumeElements().begin(), volumeElementGraph.getVolumeElements().end(), volumeElement, VolumeElementCompare);
        if(volumeElementsIter != volumeElementGraph.getVolumeElements().end() && volumeElementsIter->getVolumeElementConfiguration() == volumeElementConfiguration) {
            int elementSubspaceIndex = volumeElementsIter->getElementSubspaceIndex();
            vector<int>::iterator positiveVolumeElementSubspaceIndicesIter;
            positiveVolumeElementSubspaceIndicesIter = lower_bound(volumeElementGraph.getPositiveVolumeElementSubspaceIndices().begin(), volumeElementGraph.getPositiveVolumeElementSubspaceIndices().end(), elementSubspaceIndex);
            if(positiveVolumeElementSubspaceIndicesIter != volumeElementGraph.getPositiveVolumeElementSubspaceIndices().end() &&
                *positiveVolumeElementSubspaceIndicesIter == elementSubspaceIndex) {
                subSpaceIndex = positiveVolumeElementSubspaceIndicesIter - volumeElementGraph.getPositiveVolumeElementSubspaceIndices().begin();
            }
        }
        return subSpaceIndex;
    }
    
private:
    string _typeId;
    int _version;
    DataSource _dataSource;
    TrainedModel _trainedModel;
    vector<VolumeElementGraph> _volumeElementGraphs;
};

#endif
