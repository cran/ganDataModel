// Copyright 2021 Werner Mueller
// Released under the GPL (>= 2)

#ifndef VOLUME_ELEMENT_SUBSPACE
#define VOLUME_ELEMENT_SUBSPACE

#include <vector>

#include "inOut.h"

using namespace std;

class VolumeElementElementSubspace {
public:
    VolumeElementElementSubspace() {}
    vector<int>& getVolumeElementIndices() {
        return  _volumeElementIndices;
    }
    void setSubspaceIndex(int subspaceIndex) {
        _subspaceIndex = subspaceIndex;
    }
    int getSubspaceIndex() {
        return _subspaceIndex;
    }
    
    void write(ofstream& os) {
        InOut::Write(os, _volumeElementIndices);
        InOut::Write(os, _subspaceIndex);
    }
    
    void read(ifstream& os) {
        InOut::Read(os, _volumeElementIndices);
        InOut::Read(os, _subspaceIndex);
    }
    
private:
    vector<int> _volumeElementIndices;
    int _subspaceIndex;
};

class VolumeElementSubspace {
public:
    VolumeElementSubspace() {}
    vector<int>& getElementSubspaceIndices() {
        return  _elementSubspaceIndices;
    }
    
    void write(ofstream& os) {
        InOut::Write(os, _name);
        InOut::Write(os, _elementSubspaceIndices);
    }
    
    void read(ifstream& os) {
        InOut::Read(os, _name);
        InOut::Read(os, _elementSubspaceIndices);
    }
    
private:
    string _name;
    vector<int> _elementSubspaceIndices;
};

#endif
