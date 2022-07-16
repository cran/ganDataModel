// Copyright 2021 Werner Mueller
// Released under the GPL (>= 2)

#ifndef VOLUME_ELEMENT
#define VOLUME_ELEMENT

#include <vector>

#include "generativeData.h"
#include "vpTree.h"

#define GD_RCPP

#ifdef GD_RCPP
#include <Rcpp.h>
#endif

using namespace std;

typedef vector<bool> VolumeElementConfiguration; 

void convert(const vector<float>& a, vector<bool>& b) {
    b.resize(a.size());
    for(int i = 0; i < (int)a.size(); i++) {
        b[i] = static_cast<bool>(a[i]);
    }
}

void convert(const vector<bool>& a, vector<float>& b) {
    b.resize(a.size());
    for(int i = 0; i < (int)a.size(); i++) {
        b[i] = static_cast<float>(a[i]);
    }
}

class VolumeElement {
public:
    VolumeElement() {}
    VolumeElement(const VolumeElementConfiguration& volumeElementConfiguration): _volumeElementConfiguration(volumeElementConfiguration) {}
    VolumeElement(const VolumeElementConfiguration& volumeElementConfiguration, const vector<int>& generativeDataIndices): _volumeElementConfiguration(volumeElementConfiguration), _generativeDataIndices(generativeDataIndices) {}
    
    VolumeElementConfiguration& getVolumeElementConfiguration() {
        return _volumeElementConfiguration;
    }
    const VolumeElementConfiguration& getVolumeElementConfiguration() const {
        return _volumeElementConfiguration;
    }
    vector<VpElement>& getPositiveAdjacentVolumeElements() {
        return _positiveAdjacentVolumeElements;
    }
    vector<VpElement>& getNegativeAdjacentVolumeElements() {
        return _negativeAdjacentVolumeElements;
    }
    bool isBoundaryElement() {
        if(isIsolatedElement()) {
            return true;
        }
        
        if(getPositive()) {
            if(getNegativeAdjacentVolumeElements().size() > 0) {
                return true;
            } else {
                return false;
            }
        } else {
            if(getPositiveAdjacentVolumeElements().size() > 0) {
                return true;
            } else {
                return false;
            }
        }
    }
    bool isIsolatedElement() {
        if(getPositiveAdjacentVolumeElements().size() == 0 &&
           getNegativeAdjacentVolumeElements().size() == 0) {
            return true;
        } else {
            return false;
        }
    }
    
    int getElementSubspaceIndex() {
            return _elementSubspaceIndex;
    }
    void setElementSubspaceIndex(int elementSubspaceIndex) {
        _elementSubspaceIndex = elementSubspaceIndex;
    }
    vector<float> getNumberVector() {
        vector<float> numberVector;
        convert(_volumeElementConfiguration, numberVector);
        return  numberVector;
    };
    bool getPositive() {
        return _volumeElementConfiguration[_volumeElementConfiguration.size() - 1];
    }
    
    void write(ofstream& os) {
        InOut::Write(os, _volumeElementConfiguration);
        
        int size = _positiveAdjacentVolumeElements.size();
        InOut::Write(os, size);
        for(int i = 0; i < (int)_positiveAdjacentVolumeElements.size(); i++) {
            _positiveAdjacentVolumeElements[i].write3(os);
        }
        size = _negativeAdjacentVolumeElements.size();
        InOut::Write(os, size);
        for(int i = 0; i < (int)_negativeAdjacentVolumeElements.size(); i++) {
            _negativeAdjacentVolumeElements[i].write3(os);
        }
        
        InOut::Write(os, _elementSubspaceIndex);
        
        InOut::Write(os, _generativeDataIndices);
    }
    void read(ifstream& is, int dim) {
        InOut::Read(is, _volumeElementConfiguration, dim);
        
        int size = 0;
        InOut::Read(is, size);
        _positiveAdjacentVolumeElements.resize(size);
        for(int i = 0; i < (int)_positiveAdjacentVolumeElements.size(); i++) {
            _positiveAdjacentVolumeElements[i].read3(is);
        }
        size = 0;
        InOut::Read(is, size);
        _negativeAdjacentVolumeElements.resize(size);
        for(int i = 0; i < (int)_negativeAdjacentVolumeElements.size(); i++) {
            _negativeAdjacentVolumeElements[i].read3(is);
        }
        
        InOut::Read(is, _elementSubspaceIndex);
        
        InOut::Read(is, _generativeDataIndices);
    }
    vector<int>& getGenerativeDataIndices() {
        return _generativeDataIndices;
    }
private:
    VolumeElementConfiguration _volumeElementConfiguration;
    vector<VpElement> _positiveAdjacentVolumeElements;
    vector<VpElement> _negativeAdjacentVolumeElements;
    int _elementSubspaceIndex;
    vector<int> _generativeDataIndices;
};

bool VolumeElementCompare(const VolumeElement& a, const VolumeElement& b) {
    if(a.getVolumeElementConfiguration() < b.getVolumeElementConfiguration()) {
        return true;
    }
    return false;
};

#endif
