// Copyright 2021 Werner Mueller
// Released under the GPL (>= 2)

#ifndef VOLUME_ELEMENT_GRAPH
#define VOLUME_ELEMENT_GRAPH

#include <vector>

#include "generativeData.h"
#include "volumeElement.h"
#include "volumeElementSubspace.h"
#include "vpTree.h"

#define GD_RCPP

#ifdef GD_RCPP
#include <Rcpp.h>
#endif

using namespace std;

typedef vector<bool> VolumeElementConfiguration; 

class VpVolumeElementConfigurations : public VpTreeData {
public:
    VpVolumeElementConfigurations(vector<VolumeElement>& volumeElements): _volumeElements(volumeElements) {
        ;
    }

    void clear() {
        _volumeElements.clear();
    }
    virtual vector<float> getNumberVector(int i) {
        return _volumeElements[i].getNumberVector();
    }
    virtual int getSize() {
        return _volumeElements.size();  
    }
    vector<VolumeElement>& getVolumeElements() {
        return _volumeElements;
    }
        
private:
    vector<VolumeElement>& _volumeElements;
};

struct ElementSubspaceIndicesSizesCompare{
    bool operator()(const pair<int, int>& a, const pair<int, int>& b) {
        if(a.second > b.second) {
            return true;
        }
        if(a.second == b.second) {
            if(a.first < b.first) {
                return true;
            }
        }
        return false;
    }
};

class VolumeElementGraph {
public:
    typedef map<VolumeElementConfiguration, vector<int>> VolumeElementConfigurationMap;
    
    VolumeElementGraph() : _level(0), _pVpTree(0), _pVpVolumeElementConfigurations(0) {
        ;
    }
    VolumeElementGraph(float level) : _level(level), _pVpTree(0), _pVpVolumeElementConfigurations(0) {
        ;
    }
    VolumeElementGraph(const VolumeElementGraph& volumeElementGraph) {
        *this = volumeElementGraph;
    }
    VolumeElementGraph& operator=(const VolumeElementGraph& volumeElementGraph) {
        _level = volumeElementGraph._level;
        
        _volumeElements = volumeElementGraph._volumeElements;
        
        _pVpTree = 0;
        _l1Distance = volumeElementGraph._l1Distance;
        _pVpVolumeElementConfigurations = 0;
        
        _volumeElementElementSubspaces = volumeElementGraph._volumeElementElementSubspaces;
        _volumeElementSubspaces= volumeElementGraph._volumeElementSubspaces;
        
        _positiveVolumeElementSubspaceIndices = volumeElementGraph._positiveVolumeElementSubspaceIndices;
        _negativeVolumeElementSubspaceIndices = volumeElementGraph._negativeVolumeElementSubspaceIndices;
        
        _generativeDataVolumeElementIndices = volumeElementGraph._generativeDataVolumeElementIndices;
        
        return *this;
    }
    ~VolumeElementGraph(){
        delete _pVpTree;
        delete _pVpVolumeElementConfigurations;
    }
    float getLevel() {
        return _level;
    }
    void setLevel(float level) {
        _level = level;
    }
    void addVolumeElements(vector<float>& volumeElementValues, std::vector<int>& dimensions, int indexBegin, float level = 0, Progress* pProgress = 0) {
        int sumDimensions = 0;
        for(int i = 0; i < (int)dimensions.size(); i++) {
            sumDimensions += dimensions[i];
        }
        if(volumeElementValues.size() % sumDimensions != 0) {
            throw string(cInvalidVectorSize);
        }
        for(int i = 0; i < (int)volumeElementValues.size() / sumDimensions; i++) {
            addVolumeElement(volumeElementValues, i * sumDimensions, sumDimensions, indexBegin + i, level);
            
            if(pProgress != 0) {
                (*pProgress)(indexBegin + i + 1);
            }
        }
    }
    void addVolumeElement(vector<float>& volumeElementValues, int offset, int dimension, int index, float level = 0) {
        vector<float> v;
        v.insert(v.end(), volumeElementValues.begin() + offset, volumeElementValues.begin() + offset + dimension);
       
        for(int i = 0; i < dimension - 1; i++) {
            if(v[i] >= 0.0) {
                v[i] = 1;
            } else {
                v[i] = 0;
            }
        }
        if(v[dimension - 1] >= level) {
            v[dimension - 1] = 1;
        } else {
            v[dimension - 1] = 0;
        }
        
        VolumeElementConfiguration vec;
        convert(v, vec);
        
        _volumeElementConfigurationMap[vec].push_back(index);
        
    }
    VolumeElementConfiguration getVolumeElementConfiguration(vector<float>& volumeElementValues, int offset, int dimension, float level = 0) {
        vector<float> v;
        v.insert(v.end(), volumeElementValues.begin() + offset, volumeElementValues.begin() + offset + dimension);
      
        for(int i = 0; i < dimension - 1; i++) {
            if(v[i] >= 0.0) {
                v[i] = 1;
            } else {
                v[i] = 0;
            }
        }
        if(v[dimension - 1] >= level) {
            v[dimension - 1] = 1;
        } else {
            v[dimension - 1] = 0;
        }
      
        VolumeElementConfiguration vec;
        convert(v, vec);
        return vec;
    }
    
    void buildVolumeElements() {
        int dimension = 0;
        if(_volumeElementConfigurationMap.size() == 0) {
            return;
        }
        
        _volumeElements.clear();
        VolumeElementConfigurationMap::iterator volumeElementConfigurationMapIter;
        for(volumeElementConfigurationMapIter = _volumeElementConfigurationMap.begin(); volumeElementConfigurationMapIter != _volumeElementConfigurationMap.end(); ++volumeElementConfigurationMapIter) {
            _volumeElements.push_back(VolumeElement(volumeElementConfigurationMapIter->first, volumeElementConfigurationMapIter->second));
        }
        
        int maxGenerativeDataIndex = 0;
        for(int i = 0; i < (int)_volumeElements.size(); i++) {
            vector<int>& generativeDataIndices = _volumeElements[i].getGenerativeDataIndices();
            for(int j = 0; j < (int)generativeDataIndices.size(); j++) {
                if(generativeDataIndices[j] > maxGenerativeDataIndex) {
                    maxGenerativeDataIndex = generativeDataIndices[j];
                }
            }
        }
        _generativeDataVolumeElementIndices.resize(maxGenerativeDataIndex + 1, -1);
        for(int i = 0; i < (int)_volumeElements.size(); i++) {
            vector<int>& generativeDataIndices = _volumeElements[i].getGenerativeDataIndices();
            for(int j = 0; j < (int)generativeDataIndices.size(); j++) {
                _generativeDataVolumeElementIndices[generativeDataIndices[j]] = i;
            }
        }
    }
    void buildVolumeElementTree(Progress* pProgress) {
        _pVpVolumeElementConfigurations = new VpVolumeElementConfigurations(_volumeElements);
        delete pProgress;
        pProgress = new Progress(_pVpVolumeElementConfigurations->getSize());
        delete _pVpTree;
        _pVpTree = new VpTree();
        _pVpTree->build(_pVpVolumeElementConfigurations, new L1Distance(), pProgress);
    }
    bool isVolumeElementTreeBuilt() {
        if(_pVpTree != 0 && _pVpTree->isBuilt()) {
            return true;
        } else {
            return false;
        }
    }
    
    vector<VpElement> getSignedAdjacentVolumeElements(const vector<VpElement>& adjacentVolumeElements, bool positive = true) {
        vector<VpElement> signedAdjacentVolumeElements;
        for(int i = 0; i < (int)adjacentVolumeElements.size(); i++) {
            if(_volumeElements[adjacentVolumeElements[i].getIndex()].getPositive() == positive) {
                signedAdjacentVolumeElements.push_back((adjacentVolumeElements[i]));
            }
        }
        return signedAdjacentVolumeElements;
    }
    void buildVolumeElementGraph(int i, int kDistances, int k, bool boundary = false) {
        if(boundary && !_volumeElements[i].isBoundaryElement()) {
            return;
        }
            
        vector<VpElement> adjacentVolumeElements = gedAdjacentVolumeElements(i, kDistances, k);
        VpElementCompare vpElementCompare;
        sort(adjacentVolumeElements.begin(), adjacentVolumeElements.end(), vpElementCompare);
            
        vector<VpElement> positiveAdjacentVolumeElements = getSignedAdjacentVolumeElements(adjacentVolumeElements, true);
        vector<VpElement> negativeAdjacentVolumeElements = getSignedAdjacentVolumeElements(adjacentVolumeElements, false);
        if(positiveAdjacentVolumeElements.size() > _volumeElements[i].getPositiveAdjacentVolumeElements().size()) {
            _volumeElements[i].getPositiveAdjacentVolumeElements() = positiveAdjacentVolumeElements;
        }
        if(negativeAdjacentVolumeElements.size() > _volumeElements[i].getNegativeAdjacentVolumeElements().size()) {
            _volumeElements[i].getNegativeAdjacentVolumeElements() = negativeAdjacentVolumeElements;
        }
    }
    void buildVolumeElementGraph(int kDistances, int k, bool boundary = false, Progress* pProgress = 0) {
        for(int i = 0; i < (int)_volumeElements.size(); i++) {
            buildVolumeElementGraph(i, kDistances, k, boundary);
        
            if(pProgress != 0) {
                (*pProgress)(i + 1);
            }
        }
    }
    void buildVolumeElementGraphIterative(int kDistances, int k, int iterations, bool boundary = false, Progress* pProgress = 0) {
        delete pProgress;
        pProgress = new Progress(iterations * _volumeElements.size());
        
        int kNearestNeighbours = kDistances;
        for(int i = 1; i <= iterations; i++) {
            buildVolumeElementGraph(kNearestNeighbours, k, boundary, pProgress);
            kNearestNeighbours = kNearestNeighbours * 2;
            
            pProgress->setOffset(i * _volumeElements.size());
        }
    }
    void buildVolumeElementGraphElementSubspace(int kDistances, int k, int maxSize, bool boundary = false) {
        buildElementSubspaces();
        for(int i = 0; i < (int)getVolumeElementElementSubspaces().size(); i++) {
            if(getVolumeElementElementSubspaces()[i].getVolumeElementIndices().size() <= maxSize) {
                for(int j = 0; j < getVolumeElementElementSubspaces()[i].getVolumeElementIndices().size(); j++) {
                    int index = getVolumeElementElementSubspaces()[i].getVolumeElementIndices()[j];
                    buildVolumeElementGraph(index, kDistances, k, boundary);
                }
            }
        }
    }
    vector<VolumeElement>& getVolumeElements() {
        return _volumeElements;
    }
    vector<VolumeElementElementSubspace>& getVolumeElementElementSubspaces() {
        return _volumeElementElementSubspaces;
    }
    vector<VolumeElementSubspace>& getVolumeElementSubspaces() {
        return _volumeElementSubspaces;
    }
    vector<int>& getPositiveVolumeElementSubspaceIndices() {
        return _positiveVolumeElementSubspaceIndices;
    }
    vector<int>& getNegativeVolumeElementSubspaceIndices() {
      return _negativeVolumeElementSubspaceIndices;
    }
    
    vector<VpElement> gedAdjacentVolumeElements(int index, int kDistances, int k) {
        vector<VpElement> nearestNeighbours;
        
        vector<float> target = _volumeElements[index].getNumberVector();
        _pVpTree->search(target, kDistances, k, nearestNeighbours);
        vector<VpElement> adjacentElements;
        
        for(int i = 0; i < (int)nearestNeighbours.size(); i++) {
            if(nearestNeighbours[i].getDistance() == 0) {
                continue;
            }
            if(nearestNeighbours[i].getDistance() == 1) {
                adjacentElements.push_back(nearestNeighbours[i]);
                continue;
            }
            
            vector<float> a = _volumeElements[nearestNeighbours[i].getIndex()].getNumberVector();
            float da = nearestNeighbours[i].getDistance();
            bool adjacent = true;
            for(int j = 0; j < i; j++) {
                if(nearestNeighbours[j].getDistance() == 0) {
                    continue;
                }
                if(j == i) {
                    continue;
                }
                vector<float> b = _volumeElements[nearestNeighbours[j].getIndex()].getNumberVector();
                
                float db = nearestNeighbours[j].getDistance();
                if(da == db) {
                    continue;
                }
                
                if(db + _l1Distance(a, b) == da) {
                    adjacent = false;
                    break;
                }
            }
            if(adjacent) {
                adjacentElements.push_back(nearestNeighbours[i]);
            }
            
        }
        return adjacentElements;
    }

    VpTree* getVpTree() {
        return _pVpTree;    
    }
    
    VpVolumeElementConfigurations* getVpVolumeElementConfigurations() {
        return _pVpVolumeElementConfigurations;
    }
    
    void buildElementSubspacesLoop(int index, bool positive, int elementSubspaceIndex) {
        vector<int> stack;
        stack.push_back(index);
      
        while(!stack.empty()) {
            int i = stack.back();
            stack.pop_back();
            if(_volumeElements[i].getElementSubspaceIndex() == -1) {
                _volumeElements[i].setElementSubspaceIndex(elementSubspaceIndex);
                for(int j = 0; j < (int)_volumeElements[i].getPositiveAdjacentVolumeElements().size(); j++) {
                    if(_volumeElements[_volumeElements[i].getPositiveAdjacentVolumeElements()[j].getIndex()].getPositive() == positive) {
                        stack.push_back(_volumeElements[i].getPositiveAdjacentVolumeElements()[j].getIndex());
                    }
                }
                for(int j = 0; j < (int)_volumeElements[i].getNegativeAdjacentVolumeElements().size(); j++) {
                    if(_volumeElements[_volumeElements[i].getNegativeAdjacentVolumeElements()[j].getIndex()].getPositive() == positive) {
                        stack.push_back(_volumeElements[i].getNegativeAdjacentVolumeElements()[j].getIndex());
                    }
                }
            }
        }
    }
    
    void buildElementSubspaces() {
        for(int i = 0; i < (int)_volumeElements.size(); i++) {
            _volumeElements[i].setElementSubspaceIndex(-1);
        }
      
        int elementSubspaceIndex = 0;
        for(int i = 0; i < (int)_volumeElements.size(); i++) {
            if(_volumeElements[i].getElementSubspaceIndex() == -1) {
                buildElementSubspacesLoop(i, _volumeElements[i].getPositive(), elementSubspaceIndex);
          
                int c = 0;
                do{
                    c = 0;
                    for(int j = i + 1; j < (int)_volumeElements.size(); j++) {
                    if(_volumeElements[j].getElementSubspaceIndex() == -1 &&
                        _volumeElements[j].getPositive() == _volumeElements[i].getPositive()) {
                        for(int k = 0; k < (int)_volumeElements[j].getPositiveAdjacentVolumeElements().size(); k++) {
                            if(_volumeElements[_volumeElements[j].getPositiveAdjacentVolumeElements()[k].getIndex()].getPositive() == _volumeElements[i].getPositive() &&
                            _volumeElements[_volumeElements[j].getPositiveAdjacentVolumeElements()[k].getIndex()].getElementSubspaceIndex() == elementSubspaceIndex) {
                            buildElementSubspacesLoop(j, _volumeElements[i].getPositive(), elementSubspaceIndex);
                            c++;
                        }
                    }
                    for(int k = 0; k < (int)_volumeElements[j].getNegativeAdjacentVolumeElements().size(); k++) {
                        if(_volumeElements[_volumeElements[j].getNegativeAdjacentVolumeElements()[k].getIndex()].getPositive() ==_volumeElements[i].getPositive() &&
                            _volumeElements[_volumeElements[j].getNegativeAdjacentVolumeElements()[k].getIndex()].getElementSubspaceIndex() == elementSubspaceIndex) {
                            buildElementSubspacesLoop(j, _volumeElements[i].getPositive(), elementSubspaceIndex);
                            c++;
                                }
                            }
                        }
                    }
                } while (c > 0);
          
                elementSubspaceIndex++;
            }
        }
      
        _volumeElementElementSubspaces.clear();
        _volumeElementElementSubspaces.resize(elementSubspaceIndex);
        for(int i = 0; i < (int)_volumeElements.size(); i++) {
            int elementSubspaceIndex = _volumeElements[i].getElementSubspaceIndex();
            _volumeElementElementSubspaces[elementSubspaceIndex].getVolumeElementIndices().push_back(i);
        }
    }
    int buildSubspaces(int minSubspaceSize) {
        buildElementSubspaces();
        
        _volumeElementSubspaces.clear();
    
        int subspaceIndex = 0;
        
        vector<pair<int, int>> elementSubspaceIndicesSizes;
        for(int i = 0; i < (int)_volumeElementElementSubspaces.size(); i++) {
            int elementSubspaceSize = getVolueElementElementSubspaceSize(i);
            if(elementSubspaceSize >= minSubspaceSize) {
                elementSubspaceIndicesSizes.push_back(make_pair(i, elementSubspaceSize));
            }
        }
        
        _volumeElementSubspaces.resize(elementSubspaceIndicesSizes.size());
        ElementSubspaceIndicesSizesCompare elementSubspaceIndicesSizesCompare;
        sort(elementSubspaceIndicesSizes.begin(), elementSubspaceIndicesSizes.end(), elementSubspaceIndicesSizesCompare);
        for(int i = 0; i < (int)elementSubspaceIndicesSizes.size(); i++) {
            VolumeElementSubspace volumeElementSubspace;
            volumeElementSubspace.getElementSubspaceIndices().push_back(elementSubspaceIndicesSizes[i].first);
            _volumeElementSubspaces[i] = volumeElementSubspace;
          
            _volumeElementElementSubspaces[elementSubspaceIndicesSizes[i].first].setSubspaceIndex(subspaceIndex);
          
            subspaceIndex++;
        }
        
        for(int i = 0; i < (int)_volumeElementElementSubspaces.size(); i++) {
            int elementSubspaceSize =  getVolueElementElementSubspaceSize(i);
            if(elementSubspaceSize < minSubspaceSize) {
                 set<int> adjacentElementSubspaceIndices = getAdjacentVolumeElementElementSubspaces(i);
    
                if(adjacentElementSubspaceIndices.size() == 0) {
                    continue;
                }
                
                vector<pair<int, int>> elementSubspaceIndicesSizes;
                for(set<int>::const_iterator iter =  adjacentElementSubspaceIndices.begin(); iter !=  adjacentElementSubspaceIndices.end(); ++iter) {
                    elementSubspaceIndicesSizes.push_back(make_pair(*iter, getVolueElementElementSubspaceSize(*iter)));
                }
                
                ElementSubspaceIndicesSizesCompare elementSubspaceIndicesSizesCompare;
                sort(elementSubspaceIndicesSizes.begin(), elementSubspaceIndicesSizes.end(), elementSubspaceIndicesSizesCompare);
                
                if(elementSubspaceIndicesSizes[0].second < minSubspaceSize) {
                    continue;
                }
                
                int subspaceIndex = _volumeElementElementSubspaces[elementSubspaceIndicesSizes[0].first].getSubspaceIndex();
                _volumeElementSubspaces[subspaceIndex].getElementSubspaceIndices().push_back(i);
            }
        };
        
        _positiveVolumeElementSubspaceIndices.clear();
        for(int i = 0; i < (int)_volumeElementSubspaces.size(); i++) {
            if(getSubspacePositve(i, true)) {
                _positiveVolumeElementSubspaceIndices.push_back(i);
            }
        }
        
        _negativeVolumeElementSubspaceIndices.clear();
        for(int i = 0; i < (int)_volumeElementSubspaces.size(); i++) {
            if(getSubspacePositve(i, false)) {
                _negativeVolumeElementSubspaceIndices.push_back(i);
            }
        }
        
        return _volumeElementSubspaces.size();
    }
    set<int> getAdjacentVolumeElementElementSubspaces(int elementSubspaceIndex) {
        set<int> adjacentVolumeElementElementSubspaceIndices;
        vector<int>& volumeElementIndices = _volumeElementElementSubspaces[elementSubspaceIndex].getVolumeElementIndices();
        for(int j = 0; j < volumeElementIndices.size(); j++) {
            int volumeElementIndex = volumeElementIndices[j];
            if(_volumeElements[volumeElementIndex].isBoundaryElement()) {
                if(_volumeElements[volumeElementIndex].getPositive()) {
                    for(int k = 0; k < (int)_volumeElements[volumeElementIndex].getNegativeAdjacentVolumeElements().size(); k++) {
                        int adjacentVolumeElementIndex = _volumeElements[volumeElementIndex].getNegativeAdjacentVolumeElements()[k].getIndex();
                        int adjacentElementSubspaceIndex = _volumeElements[adjacentVolumeElementIndex].getElementSubspaceIndex();
                        adjacentVolumeElementElementSubspaceIndices.insert(adjacentElementSubspaceIndex);
                    }
                } else {
                    for(int k = 0; k < (int)_volumeElements[volumeElementIndex].getPositiveAdjacentVolumeElements().size(); k++) {
                        int adjacentVolumeElementIndex = _volumeElements[volumeElementIndex].getPositiveAdjacentVolumeElements()[k].getIndex();
                        int adjacentElementSubspaceIndex = _volumeElements[adjacentVolumeElementIndex].getElementSubspaceIndex();
                        adjacentVolumeElementElementSubspaceIndices.insert(adjacentElementSubspaceIndex);
                    }
                }
            }
        }
    
        return adjacentVolumeElementElementSubspaceIndices;
    }
    int getVolueElementElementSubspaceSize(int elementSubspaceIndex) {
        int elementSubspaceSize = 0;
        vector<int>& volumeElementIndices = _volumeElementElementSubspaces[elementSubspaceIndex].getVolumeElementIndices();
        for(int i = 0; i < (int)volumeElementIndices.size(); i++) {
            elementSubspaceSize += _volumeElements[volumeElementIndices[i]].getGenerativeDataIndices().size();
        }
        return elementSubspaceSize;
    }
    bool getElemntSubspacePositve(int i, bool positive = true) {
        if(_volumeElements[_volumeElementElementSubspaces[i].getVolumeElementIndices()[0]].getPositive() == positive) {
            return true;
        } else {
            return false;
        }
    }
    bool getSubspacePositve(int i, bool positive = true) {
        VolumeElementSubspace& volumeElementSubspace = _volumeElementSubspaces[i];
        int index = volumeElementSubspace.getElementSubspaceIndices()[0];
        return getElemntSubspacePositve(index, positive);
    }
    
    vector<int> getGenerativeDataVolumeElementIndices() {
        return _generativeDataVolumeElementIndices;
    }
    
    void write(ofstream& os) {
        InOut::Write(os, _level);
        
        int dim = 0;
        if(_volumeElements.size() > 0) {
            dim = _volumeElements[0].getVolumeElementConfiguration().size();
        }
        InOut::Write(os, dim);
        
        int size = _volumeElements.size();
        InOut::Write(os, size);
        for(int i = 0; i < (int)_volumeElements.size(); i++) {
            _volumeElements[i].write(os);
        }
        
        size = _volumeElementElementSubspaces.size();
        InOut::Write(os, size);
        for(int i = 0; i < (int)_volumeElementElementSubspaces.size(); i++) {
            _volumeElementElementSubspaces[i].write(os);
        }
        
        size = _volumeElementSubspaces.size();
        InOut::Write(os, size);
        for(int i = 0; i < (int)_volumeElementSubspaces.size(); i++) {
            _volumeElementSubspaces[i].write(os);
        }
        
        InOut::Write(os, _positiveVolumeElementSubspaceIndices);
        InOut::Write(os, _negativeVolumeElementSubspaceIndices);
        
        InOut::Write(os, _generativeDataVolumeElementIndices);
    }
    void read(ifstream& is) {
        _volumeElements.clear();
        
        InOut::Read(is, _level);
        
        int dim =0;
        InOut::Read(is, dim);
        
        int size = _volumeElements.size();
        InOut::Read(is, size);
        _volumeElements.resize(size);
        for(int i = 0; i < (int)_volumeElements.size(); i++) {
            _volumeElements[i].read(is, dim);
        }
        
        size = _volumeElementElementSubspaces.size();
        InOut::Read(is, size);
        _volumeElementElementSubspaces.resize(size);
        for(int i = 0; i < (int)_volumeElementElementSubspaces.size(); i++) {
            _volumeElementElementSubspaces[i].read(is);
        }
        
        size = _volumeElementSubspaces.size();
        InOut::Read(is, size);
        _volumeElementSubspaces.resize(size);
        for(int i = 0; i < (int)_volumeElementSubspaces.size(); i++) {
            _volumeElementSubspaces[i].read(is);
        }
        
        InOut::Read(is, _positiveVolumeElementSubspaceIndices);
        InOut::Read(is, _negativeVolumeElementSubspaceIndices);
        
        InOut::Read(is, _generativeDataVolumeElementIndices);
    }
    
private:
    float _level;
    vector<VolumeElement> _volumeElements;
    
    VolumeElementConfigurationMap _volumeElementConfigurationMap;
    VpTree* _pVpTree;
    L1Distance _l1Distance;
    VpVolumeElementConfigurations* _pVpVolumeElementConfigurations;
    
    vector<VolumeElementElementSubspace> _volumeElementElementSubspaces;
    vector<VolumeElementSubspace> _volumeElementSubspaces;
    vector<int> _positiveVolumeElementSubspaceIndices;
    vector<int> _negativeVolumeElementSubspaceIndices;
    
    vector<int> _generativeDataVolumeElementIndices;
};

#endif
