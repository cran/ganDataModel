// Copyright 2021 Werner Mueller
// Released under the GPL (>= 2)

#ifndef METRIC_SUBSPACE_RELATION
#define METRIC_SUBSPACE_RELATION

#include <vector>

#define GD_RCPP

#ifdef GD_RCPP
#include <Rcpp.h>
#endif

using namespace std;

class MetricSubspaceEntry {
public:
    MetricSubspaceEntry() {
        ;
    }
    MetricSubspaceEntry(float level, int metricSubspaceIndex, int metricSubspaceSize):  _level(level), _metricSubspaceIndex(metricSubspaceIndex), _metricSubspaceSize(metricSubspaceSize) {
        ;
    }
    float getLevel() const {
        return _level;
    }
    int getMetricSubspaceIndex() const {
        return _metricSubspaceIndex;
    }
    int getMetricSubspaceSize() const {
        return _metricSubspaceSize;
    }
    vector<int>& getLabels() {
        return _labels;
    }
    vector<int>& getMetricSubspaceIndices() {
        return _metricSubspaceIndices;
    }
    string& getLabel() {
        return _label;
    }
    void setLabel(const string& label) {
        _label = label;
    }
  
private:
    float _level;
    int _metricSubspaceIndex;
    int _metricSubspaceSize;
    vector<int> _labels;
    vector<int> _metricSubspaceIndices;
    string _label;
};

struct MetricSubspaceEntryCompare{
    bool operator()(const MetricSubspaceEntry& a, const MetricSubspaceEntry& b) {
        if(a.getLevel() < b.getLevel()) {
            return true;
        }
        if(a.getLevel() == b.getLevel()) {
            if(a.getMetricSubspaceSize() > b.getMetricSubspaceSize()) {
                return true;
            } 
            if(a.getMetricSubspaceSize() == b.getMetricSubspaceSize()) {
                if(a.getMetricSubspaceIndex() < b.getMetricSubspaceIndex()) {
                    return true;
                }
            }
        }
        return false;
    }
};

class MetricSubspaceRelationEntry {
public:
    MetricSubspaceRelationEntry() {
        ;
    }
    MetricSubspaceRelationEntry(MetricSubspaceEntry& lMetricSubspaceEntry, MetricSubspaceEntry& rMetricSubspaceEntry):
        _lMetricSubspaceEntry(lMetricSubspaceEntry), _rMetricSubspaceEntry(rMetricSubspaceEntry) {
        ;
    }
    MetricSubspaceEntry& getLMetricSubspaceEntry() {
        return _lMetricSubspaceEntry;
    
    }
    const MetricSubspaceEntry& getLMetricSubspaceEntry() const {
        return _lMetricSubspaceEntry;
    } 
    MetricSubspaceEntry& getRMetricSubspaceEntry() {
        return _rMetricSubspaceEntry;
    } 
    const MetricSubspaceEntry& getRMetricSubspaceEntry() const {
        return _rMetricSubspaceEntry;
    } 
  
private:
    MetricSubspaceEntry _lMetricSubspaceEntry;
    MetricSubspaceEntry _rMetricSubspaceEntry;
};

struct MetricSubspaceRelationEntryCompare{
    bool operator()(const MetricSubspaceRelationEntry& a, const MetricSubspaceRelationEntry& b) {
        if(a.getLMetricSubspaceEntry().getLevel() < b.getLMetricSubspaceEntry().getLevel()) {
            return true;
        }
        if(a.getLMetricSubspaceEntry().getLevel() == b.getLMetricSubspaceEntry().getLevel()) {
            if(a.getLMetricSubspaceEntry().getMetricSubspaceSize() > b.getLMetricSubspaceEntry().getMetricSubspaceSize()) {
                return true;
            } 
            if(a.getLMetricSubspaceEntry().getMetricSubspaceSize() == b.getLMetricSubspaceEntry().getMetricSubspaceSize()) {
                if(a.getLMetricSubspaceEntry().getMetricSubspaceIndex() < b.getLMetricSubspaceEntry().getMetricSubspaceIndex()) {
                    return true;
                }
                if(a.getLMetricSubspaceEntry().getMetricSubspaceIndex() == b.getLMetricSubspaceEntry().getMetricSubspaceIndex()) {
                    if(a.getRMetricSubspaceEntry().getLevel() < b.getRMetricSubspaceEntry().getLevel()) {
                        return true;
                    }
                    if(a.getRMetricSubspaceEntry().getLevel() == b.getRMetricSubspaceEntry().getLevel()) {
                        if(a.getRMetricSubspaceEntry().getMetricSubspaceSize() > b.getRMetricSubspaceEntry().getMetricSubspaceSize()) {
                            return true;
                        }
                        if(a.getRMetricSubspaceEntry().getMetricSubspaceSize() == b.getRMetricSubspaceEntry().getMetricSubspaceSize()) {
                            if(a.getRMetricSubspaceEntry().getMetricSubspaceIndex() < b.getRMetricSubspaceEntry().getMetricSubspaceIndex()) {
                                return true;
                            }         
                        }
                    }
                }
            }
        }
        return false;
    }
};

class MetricSubspaceRelation {
public:
    MetricSubspaceRelation() {
        ;
    }
    vector<MetricSubspaceRelationEntry>& getMetricSubspaceRelationEntries() {
        return _metricSubspaceRelationEntries;
    }
    vector<MetricSubspaceEntry>& getMetricSubspaceEntries() {
        return _metricSubspaceEntries;
    }
    void sortMetricSubspaceEntries() {
        MetricSubspaceEntryCompare metricSubspaceEntryCompare;
        sort(_metricSubspaceEntries. begin(), _metricSubspaceEntries.end(), metricSubspaceEntryCompare);
        MetricSubspaceRelationEntryCompare metricSubspaceRelationEntryCompare;
        sort(_metricSubspaceRelationEntries. begin(), _metricSubspaceRelationEntries.end(), metricSubspaceRelationEntryCompare);
    }
    void clearMetricSubspaceRelation() {
        _metricSubspaceEntries.clear();
        _metricSubspaceRelationEntries.clear();
    }
  
    void createLabels(float level) {
        int label = 0;
      
        map<pair<float, int>, int> entryMap;
        for(int i = 0; i < (int)_metricSubspaceEntries.size(); i++) {
            if(_metricSubspaceEntries[i].getLevel() == level) {
                _metricSubspaceEntries[i].getLabels().push_back(label);
                _metricSubspaceEntries[i].getMetricSubspaceIndices().push_back(_metricSubspaceEntries[i].getMetricSubspaceIndex());
            
                entryMap[make_pair(_metricSubspaceEntries[i].getLevel(), _metricSubspaceEntries[i].getMetricSubspaceIndex())] = i;

                string l = "";
                l.push_back(static_cast<char>(label + 97));
                _metricSubspaceEntries[i].setLabel(l);
            
                if(i < (int)_metricSubspaceEntries.size() - 1) {
                    if(_metricSubspaceEntries[i + 1].getMetricSubspaceIndex() != _metricSubspaceEntries[i].getMetricSubspaceIndex()) {
                        label++;
                    }
                }
            } else {
                ;
            }
        } 
        
        for(int i = 0; i < (int)_metricSubspaceRelationEntries.size(); i++) {
            if(_metricSubspaceRelationEntries[i].getLMetricSubspaceEntry().getLevel() == level) {
                MetricSubspaceEntry& lMetricSubspaceEntry = _metricSubspaceRelationEntries[i].getLMetricSubspaceEntry();
                int index = entryMap[make_pair(lMetricSubspaceEntry.getLevel(), lMetricSubspaceEntry.getMetricSubspaceIndex())];
                MetricSubspaceEntry& metricSubspaceEntry = _metricSubspaceEntries[index];
                _metricSubspaceRelationEntries[i].getLMetricSubspaceEntry().getLabels() = metricSubspaceEntry.getLabels();
                _metricSubspaceRelationEntries[i].getLMetricSubspaceEntry().getMetricSubspaceIndices() = metricSubspaceEntry.getMetricSubspaceIndices();
            } else {
                  ;
            }
        }
        
        for(int i = 0; i < (int)_metricSubspaceRelationEntries.size(); i++) {
            if(_metricSubspaceRelationEntries[i].getLMetricSubspaceEntry().getLevel() == level) {
                createLabels(_metricSubspaceRelationEntries[i].getLMetricSubspaceEntry(), i); 
            } else {
                ;
            }
        }
    }
    void createLabels(MetricSubspaceEntry& metricSubspaceEntry, int index) {
        int label = 0;
        for(int i = index; i < (int)_metricSubspaceRelationEntries.size(); i++) {
            if(_metricSubspaceRelationEntries[i].getLMetricSubspaceEntry().getLevel() == metricSubspaceEntry.getLevel() &&
               _metricSubspaceRelationEntries[i].getLMetricSubspaceEntry().getMetricSubspaceIndex() == metricSubspaceEntry.getMetricSubspaceIndex()) {
              
                if(_metricSubspaceRelationEntries[i].getLMetricSubspaceEntry().getLabels().size() == 0) {
                    _metricSubspaceRelationEntries[i].getLMetricSubspaceEntry().getLabels() = metricSubspaceEntry.getLabels();
                }
                vector<int> labels =  _metricSubspaceRelationEntries[i].getLMetricSubspaceEntry().getLabels();
                
                labels.push_back(label);
                if(_metricSubspaceRelationEntries[i].getRMetricSubspaceEntry().getLabels().size() == 0) {
                    _metricSubspaceRelationEntries[i].getRMetricSubspaceEntry().getLabels() = labels;
                }
                label++;
              
                if(_metricSubspaceRelationEntries[i].getLMetricSubspaceEntry().getMetricSubspaceIndices().size() == 0) {
                    _metricSubspaceRelationEntries[i].getLMetricSubspaceEntry().getMetricSubspaceIndices() = metricSubspaceEntry.getMetricSubspaceIndices();
                }
                vector<int> metricSubspaceIndices =  _metricSubspaceRelationEntries[i].getLMetricSubspaceEntry().getMetricSubspaceIndices();
              
                metricSubspaceIndices.push_back(_metricSubspaceRelationEntries[i].getRMetricSubspaceEntry().getMetricSubspaceIndex());
                if(_metricSubspaceRelationEntries[i].getRMetricSubspaceEntry().getMetricSubspaceIndices().size() == 0) {
                    _metricSubspaceRelationEntries[i].getRMetricSubspaceEntry().getMetricSubspaceIndices() = metricSubspaceIndices;
                }
              
                createLabels(_metricSubspaceRelationEntries[i].getRMetricSubspaceEntry(), i);
            } else {
                ;
            }
        }
    }
    void setLabels() {
        map<pair<float, int>, string> labelMap;
        for(int i = 0; i < (int)_metricSubspaceRelationEntries.size(); i++) {
            MetricSubspaceEntry& lMetricSubspaceEntry = _metricSubspaceRelationEntries[i].getLMetricSubspaceEntry();
            MetricSubspaceEntry& rMetricSubspaceEntry = _metricSubspaceRelationEntries[i].getRMetricSubspaceEntry();
            
            vector<int>& lMetricSubspaceLabels = lMetricSubspaceEntry.getLabels();
            string lLabel = "";
            for(int j = 0; j < (int)lMetricSubspaceLabels.size(); j++) {
                char l = static_cast<char>(lMetricSubspaceLabels[j] + 97);
                lLabel.push_back(l);
            }
            vector<int>& rMetricSubspaceLabels = rMetricSubspaceEntry.getLabels();
            string rLabel = "";
            for(int j = 0; j < (int)rMetricSubspaceLabels.size(); j++) {
                char l = static_cast<char>(rMetricSubspaceLabels[j] + 97);
                rLabel.push_back(l);
            }
            
            labelMap[make_pair(lMetricSubspaceEntry.getLevel(), lMetricSubspaceEntry.getMetricSubspaceIndex())] = lLabel;
            labelMap[make_pair(rMetricSubspaceEntry.getLevel(), rMetricSubspaceEntry.getMetricSubspaceIndex())] = rLabel;
        }
        
        for(int i = 0; i < (int)_metricSubspaceEntries.size(); i++) {
            map<pair<float, int>, string>::const_iterator labelMapIterator = labelMap.find(make_pair(_metricSubspaceEntries[i].getLevel(), _metricSubspaceEntries[i].getMetricSubspaceIndex()));
            if(labelMapIterator != labelMap.end()) {
                _metricSubspaceEntries[i].setLabel(labelMapIterator->second);
            }
        }
    }
  
private:
    vector<MetricSubspaceEntry> _metricSubspaceEntries;
    vector<MetricSubspaceRelationEntry> _metricSubspaceRelationEntries;
};

#endif
