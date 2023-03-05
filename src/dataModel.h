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
#include "metricSubspaceRelation.h"

using namespace std;

const string csSeparator = "_";
const string cDataFileExtension = "data-00000-of-00001";
const string cIndexFileExtension = "index";
const string cMetaFileExtension = "meta";
const string cInvalidLevel = "Invalid level";

const string cDataModelTypeId = "2da979bc-77df-4e9e-9fb2-7916e02a001c";
const string cWildCard = "*";

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
        
        buildMetricSubspaceRelation();
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
    
    int getNumberOfMetricSubspaces(float level) {
        int levelIndex = getLevelIndex(level);
        int numberOfMetricSubspaces = _volumeElementGraphs[levelIndex].getNumberOfMetricSubspaces();
        return numberOfMetricSubspaces;
    }
    
    void removeMetricSubspaces(float level) {
        for(int i = 0; i < (int)getVolumeElementGraphs().size(); i++) {
            if(getVolumeElementGraphs()[i].getLevel() == level) {
                getVolumeElementGraphs()[i] = getVolumeElementGraphs()[getVolumeElementGraphs().size() - 1];
                getVolumeElementGraphs().pop_back();
                
                buildMetricSubspaceRelation();
                break;
            }
        }
    }
    void addMetricSubspaceEntries(VolumeElementGraph& volumeElementGraph, MetricSubspaceRelation& metricSubspaceRelation) {
        for(int i = 0; i < (int)volumeElementGraph.getMetricSubspaces().size(); i++) {
            if(volumeElementGraph.getMetricSubspacePositve(i, true)) {
                MetricSubspaceEntry metricSubspaceEntry(volumeElementGraph.getLevel(), i, volumeElementGraph.getMetricSubspaceSize(i));                
                metricSubspaceRelation.getMetricSubspaceEntries().push_back(metricSubspaceEntry);
            }
        }
    }
    void addMetricSubspaceRelationEntries(VolumeElementGraph& lVolumeElementGraph, VolumeElementGraph& rVolumeElementGraph, MetricSubspaceRelation& metricSubspaceRelation) {
        for(int i = 0; i < (int)rVolumeElementGraph.getMetricSubspaces().size(); i++) {
            if(rVolumeElementGraph.getMetricSubspacePositve(i, true)) {
                vector<int>& rMetricSubspaceElementIndices = rVolumeElementGraph.getMetricSubspaces()[i].getMetricSubspaceElementIndices();
                int rMetricSubspaceElementIndex = rMetricSubspaceElementIndices[0];
            
                vector<int>& rVolumeElementIndices = rVolumeElementGraph.getMetricSubspaceElements()[rMetricSubspaceElementIndex].getVolumeElementIndices();
                int rVolumeElementIndex = rVolumeElementIndices[0];
                vector<int>& rVolumeElementGenerativeDataIndices = rVolumeElementGraph.getVolumeElements()[rVolumeElementIndex].getGenerativeDataIndices();
                int rGenerativeDataIndex = rVolumeElementGenerativeDataIndices[0];
                int lVolumeElementIndex = lVolumeElementGraph.getGenerativeDataVolumeElementIndices()[rGenerativeDataIndex];
                int lVolumeElementSubspaceElementIndex = lVolumeElementGraph.getVolumeElements()[lVolumeElementIndex].getMetricSubspaceElementIndex();
                int lMetricSubspaceIndex = lVolumeElementGraph.getMetricSubspaceElements()[lVolumeElementSubspaceElementIndex].getMetricSubspaceIndex();
            
                MetricSubspaceEntry lMetricSubspaceEntry(lVolumeElementGraph.getLevel(), lMetricSubspaceIndex, lVolumeElementGraph.getMetricSubspaceSize(lMetricSubspaceIndex));
                MetricSubspaceEntry rMetricSubspaceEntry(rVolumeElementGraph.getLevel(), i, rVolumeElementGraph.getMetricSubspaceSize(i));
                MetricSubspaceRelationEntry metricSubspaceRelationEntry(lMetricSubspaceEntry, rMetricSubspaceEntry);
                metricSubspaceRelation.getMetricSubspaceRelationEntries().push_back(metricSubspaceRelationEntry);
            }
        } 
    }
    void buildMetricSubspaceRelation() {
        _metricSubspaceRelation.clearMetricSubspaceRelation();
        
        vector<float> orderedLevels = getLevels();
        for(int i = 0; i < (int)orderedLevels.size(); i++) {
            int lLevelIndex = getLevelIndex(orderedLevels[i]);
            VolumeElementGraph& lVolumeElementGraph = _volumeElementGraphs[lLevelIndex];
            addMetricSubspaceEntries(lVolumeElementGraph, _metricSubspaceRelation);
            
            if(i < (int)orderedLevels.size() - 1) {
                int rLevelIndex = getLevelIndex(orderedLevels[i + 1]);
                VolumeElementGraph& rVolumeElementGraph = _volumeElementGraphs[rLevelIndex];
                addMetricSubspaceRelationEntries(lVolumeElementGraph, rVolumeElementGraph, _metricSubspaceRelation);
            }
        }
  
        _metricSubspaceRelation.sortMetricSubspaceEntries();
        //if(_metricSubspaceRelation.getMetricSubspaceRelationEntries().size() > 0) {
        //    _metricSubspaceRelation.createLabels(_metricSubspaceRelation.getMetricSubspaceRelationEntries()[0].getLMetricSubspaceEntry().getLevel());
        //    _metricSubspaceRelation.setLabels();
        //}
        if(_metricSubspaceRelation.getMetricSubspaceEntries().size() > 0) {
          _metricSubspaceRelation.createLabels(_metricSubspaceRelation.getMetricSubspaceEntries()[0].getLevel());
          _metricSubspaceRelation.setLabels();
        }
    }
    MetricSubspaceRelation& getMetricSubspaceRelation() {
        return _metricSubspaceRelation;
    }

    vector<int> getMetricSubspaceIndices(float level) {
        //int levelIndex = getLevelIndex(level);
        vector<int> metricSubspaceIndices;
        for(int i = 0; i < (int)_metricSubspaceRelation.getMetricSubspaceEntries().size(); i++) {
            MetricSubspaceEntry& metricSubspaceEntry = _metricSubspaceRelation.getMetricSubspaceEntries()[i];
            if(metricSubspaceEntry.getLevel() == level) {
                metricSubspaceIndices.push_back(metricSubspaceEntry.getMetricSubspaceIndex());
            }
        }
        return metricSubspaceIndices;
    }
    vector<int> getMetricSubspaceIndices(float level, const vector<string>& labels) {
        //int levelIndex = getLevelIndex(level);
      
        set<string> labelSet;
        for(int i = 0; i < (int)labels.size(); i++) {
            labelSet.insert(labels[i]);
        }
        vector<int> metricSubspaceIndices;
        for(int i = 0; i < (int)_metricSubspaceRelation.getMetricSubspaceEntries().size(); i++) {
            MetricSubspaceEntry& metricSubspaceEntry = _metricSubspaceRelation.getMetricSubspaceEntries()[i];
            if(metricSubspaceEntry.getLevel() == level && (labelSet.empty() || labelSet.find(cWildCard) != labelSet.end() || labelSet.find(metricSubspaceEntry.getLabel()) != labelSet.end())) {
                metricSubspaceIndices.push_back(metricSubspaceEntry.getMetricSubspaceIndex());
            }
        }
        return metricSubspaceIndices;
    }
    vector<int> getMetricSubspaceEntryIndices(float level, int metricSubspaceIndex) {
        vector<int> metricSubspaceEntryIndices;
        for(int i = 0; i < (int)_metricSubspaceRelation.getMetricSubspaceEntries().size(); i++) {
            if(_metricSubspaceRelation.getMetricSubspaceEntries()[i].getLevel() == level &&
                _metricSubspaceRelation.getMetricSubspaceEntries()[i].getMetricSubspaceSize() <= _metricSubspaceRelation.getMetricSubspaceEntries()[metricSubspaceIndex].getMetricSubspaceSize() &&
                _metricSubspaceRelation.getMetricSubspaceEntries()[i].getMetricSubspaceIndex() != metricSubspaceIndex) {
                metricSubspaceEntryIndices.push_back(i);
            }
        }
        return metricSubspaceEntryIndices;
    }
    vector<int> getMetricSubspaceEntryIndices(float level) {
        vector<int> metricSubspaceEntryIndices;
        for(int i = 0; i < (int)_metricSubspaceRelation.getMetricSubspaceEntries().size(); i++) {
            if(_metricSubspaceRelation.getMetricSubspaceEntries()[i].getLevel() == level) {
                metricSubspaceEntryIndices.push_back(i);
            }
        }
        return metricSubspaceEntryIndices;
    }
    vector<int> getMetricSubspaceGenerativeDataIndices(vector<int>& metricSubspaceEntryIndices) {
        vector<int> generativeDataIndices;
        for(int i = 0; i < (int)metricSubspaceEntryIndices.size(); i++) {
            int metricSubspaceEntryIndex = metricSubspaceEntryIndices[i];
            float metricSubspaceIndex = _metricSubspaceRelation.getMetricSubspaceEntries()[metricSubspaceEntryIndex].getMetricSubspaceIndex();
            
            float level = _metricSubspaceRelation.getMetricSubspaceEntries()[metricSubspaceEntryIndex].getLevel();
            int levelIndex = getLevelIndex(level);
            VolumeElementGraph& volumeElementGraph = getVolumeElementGraphs()[levelIndex];
            
            vector<int> lGenerativeDataIndices = volumeElementGraph.getGenerativeDataVolumeElementIndices(metricSubspaceIndex, false);
            generativeDataIndices.insert(generativeDataIndices.end(), lGenerativeDataIndices.begin(), lGenerativeDataIndices.end());
        }

        return generativeDataIndices;
    }
    vector<int> getMetricSubspaceGenerativeDataIndices(float lLevel, float rLevel, int metricSubspaceIndex) {
        vector<int> generativeDataIndices;

        vector<int> metricSubspaceEntryIndices = getMetricSubspaceEntryIndices(lLevel, metricSubspaceIndex);
        vector<int> lGenerativeDataIndices = getMetricSubspaceGenerativeDataIndices(metricSubspaceEntryIndices);
        generativeDataIndices.insert(generativeDataIndices.begin(), lGenerativeDataIndices.begin(), lGenerativeDataIndices.end());

        if(rLevel != -1) {
            metricSubspaceEntryIndices = getMetricSubspaceEntryIndices(rLevel);
            vector<int> rGenerativeDataIndices = getMetricSubspaceGenerativeDataIndices(metricSubspaceEntryIndices);
            generativeDataIndices.insert(generativeDataIndices.begin(), rGenerativeDataIndices.begin(), rGenerativeDataIndices.end());
        }
  
        return generativeDataIndices;
    }
    string getMetricSubspaceLabel(float level, int metricSubspaceIndex) {
        string label = "";
        for(int i = 0; i < (int)_metricSubspaceRelation.getMetricSubspaceEntries().size(); i++) {
            MetricSubspaceEntry& metricSubspaceEntry = _metricSubspaceRelation.getMetricSubspaceEntries()[i];
            if(metricSubspaceEntry.getLevel() == level && metricSubspaceEntry.getMetricSubspaceIndex() == metricSubspaceIndex) {
                label = metricSubspaceEntry.getLabel();
                break;
            } 
        }
        return label;
    }
  
private:
    string _typeId;
    int _version;
    DataSource _dataSource;
    TrainedModel _trainedModel;
    vector<VolumeElementGraph> _volumeElementGraphs;
    
    MetricSubspaceRelation _metricSubspaceRelation;
};

#endif
