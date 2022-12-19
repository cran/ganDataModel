// Copyright 2021 Werner Mueller
// Released under the GPL (>= 2)

#ifndef METRIC_SUBSPACE
#define METRIC_SUBSPACE

#include <vector>

#include "inOut.h"

using namespace std;

class MetricSubspaceElement {
public:
    MetricSubspaceElement() {}
    vector<int>& getVolumeElementIndices() {
        return  _volumeElementIndices;
    }
    void setMetricSubspaceIndex(int subspaceIndex) {
        _metricSubspaceIndex = subspaceIndex;
    }
    int getMetricSubspaceIndex() {
        return _metricSubspaceIndex;
    }
    
    void write(ofstream& os) {
        InOut::Write(os, _volumeElementIndices);
        InOut::Write(os, _metricSubspaceIndex);
    }
    
    void read(ifstream& os) {
        InOut::Read(os, _volumeElementIndices);
        InOut::Read(os, _metricSubspaceIndex);
    }
    
private:
    vector<int> _volumeElementIndices;
    int _metricSubspaceIndex;
};

class MetricSubspace {
public:
    MetricSubspace() {}
    vector<int>& getMetricSubspaceElementIndices() {
        return  _metricSubspaceElementIndices;
    }
    
    void write(ofstream& os) {
        InOut::Write(os, _name);
        InOut::Write(os, _metricSubspaceElementIndices);
    }
    
    void read(ifstream& os) {
        InOut::Read(os, _name);
        InOut::Read(os, _metricSubspaceElementIndices);
    }
    
private:
    string _name;
    vector<int> _metricSubspaceElementIndices;
};

#endif
