// Copyright 2021 Werner Mueller
// Released under the GPL (>= 2)

#ifndef VOLUME_ELEMENT_GRAPH
#define VOLUME_ELEMENT_GRAPH

#include <vector>

#include "generativeData.h"
#include "volumeElement.h"
#include "metricSubspace.h"
#include "vpTree.h"

#define GD_RCPP

#ifdef GD_RCPP
#include <Rcpp.h>
#endif

using namespace std;

typedef vector<bool> VolumeElementConfiguration; 

template <typename T>
class VpVolumeElementConfigurations : public VpTreeData<T> {
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
    virtual vector<T>& getReferenceNumberVector(int i) {
        return _volumeElements[i].getVolumeElementConfiguration();
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

struct SubspaceElementIndicesSizesCompare{
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
        
        _metricSubspaceElements = volumeElementGraph._metricSubspaceElements;
        _metricSubspaces= volumeElementGraph._metricSubspaces;
        
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
        _pVpVolumeElementConfigurations = new VpVolumeElementConfigurations<bool>(_volumeElements);
        delete pProgress;
        pProgress = new Progress(_pVpVolumeElementConfigurations->getSize());
        delete _pVpTree;
        _pVpTree = new VpTree<bool>();
        _pVpTree->build(_pVpVolumeElementConfigurations, new L1Distance<bool>(), pProgress);
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
    void buildVolumeElementGraphMetricSubspaceElement(int kDistances, int k, int maxSize, bool boundary = false) {
        buildMetricSubspaceElements();
        for(int i = 0; i < (int)getMetricSubspaceElements().size(); i++) {
            if(getMetricSubspaceElements()[i].getVolumeElementIndices().size() <= maxSize) {
                for(int j = 0; j < getMetricSubspaceElements()[i].getVolumeElementIndices().size(); j++) {
                    int index = getMetricSubspaceElements()[i].getVolumeElementIndices()[j];
                    buildVolumeElementGraph(index, kDistances, k, boundary);
                }
            }
        }
    }
    vector<VolumeElement>& getVolumeElements() {
        return _volumeElements;
    }
    vector<MetricSubspaceElement>& getMetricSubspaceElements() {
        return _metricSubspaceElements;
    }
    vector<MetricSubspace>& getMetricSubspaces() {
        return _metricSubspaces;
    }
    int getNumberOfMetricSubspaces() {
      int numberOfMetricSubspaces = 0;
      for(int i = 0; i < (int)getMetricSubspaces().size(); i++) {
          if(getMetricSubspacePositve(i, true)) {
              numberOfMetricSubspaces++;
          }
      }
      return numberOfMetricSubspaces;
    }
    vector<VpElement> gedAdjacentVolumeElements(int index, int kDistances, int k) {
        vector<VpElement> nearestNeighbours;
        
        vector<bool>& target = _volumeElements[index].getVolumeElementConfiguration();
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
            
            vector<bool>& a = _volumeElements[nearestNeighbours[i].getIndex()].getVolumeElementConfiguration();
            float da = nearestNeighbours[i].getDistance();
            bool adjacent = true;
            for(int j = 0; j < i; j++) {
                if(nearestNeighbours[j].getDistance() == 0) {
                    continue;
                }
                if(j == i) {
                    continue;
                }
                vector<bool>& b = _volumeElements[nearestNeighbours[j].getIndex()].getVolumeElementConfiguration();
                
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

    VpTree<bool>* getVpTree() {
        return _pVpTree;    
    }
    
    VpVolumeElementConfigurations<bool>* getVpVolumeElementConfigurations() {
        return _pVpVolumeElementConfigurations;
    }
    
    void buildMetricSubspaceElementsLoop(int index, bool positive, int metricSubspaceElementIndex) {
        vector<int> stack;
        stack.push_back(index);
      
        while(!stack.empty()) {
            int i = stack.back();
            stack.pop_back();
            if(_volumeElements[i].getMetricSubspaceElementIndex() == -1) {
                _volumeElements[i].setMetricSubspaceElementIndex(metricSubspaceElementIndex);
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
    
    void buildMetricSubspaceElements() {
        for(int i = 0; i < (int)_volumeElements.size(); i++) {
            _volumeElements[i].setMetricSubspaceElementIndex(-1);
        }
      
        int metricSubspaceElementIndex = 0;
        for(int i = 0; i < (int)_volumeElements.size(); i++) {
            if(_volumeElements[i].getMetricSubspaceElementIndex() == -1) {
                buildMetricSubspaceElementsLoop(i, _volumeElements[i].getPositive(), metricSubspaceElementIndex);
          
                int c = 0;
                do{
                    c = 0;
                    for(int j = i + 1; j < (int)_volumeElements.size(); j++) {
                    if(_volumeElements[j].getMetricSubspaceElementIndex() == -1 &&
                        _volumeElements[j].getPositive() == _volumeElements[i].getPositive()) {
                        for(int k = 0; k < (int)_volumeElements[j].getPositiveAdjacentVolumeElements().size(); k++) {
                            if(_volumeElements[_volumeElements[j].getPositiveAdjacentVolumeElements()[k].getIndex()].getPositive() == _volumeElements[i].getPositive() &&
                            _volumeElements[_volumeElements[j].getPositiveAdjacentVolumeElements()[k].getIndex()].getMetricSubspaceElementIndex() == metricSubspaceElementIndex) {
                            buildMetricSubspaceElementsLoop(j, _volumeElements[i].getPositive(), metricSubspaceElementIndex);
                            c++;
                        }
                    }
                    for(int k = 0; k < (int)_volumeElements[j].getNegativeAdjacentVolumeElements().size(); k++) {
                        if(_volumeElements[_volumeElements[j].getNegativeAdjacentVolumeElements()[k].getIndex()].getPositive() ==_volumeElements[i].getPositive() &&
                            _volumeElements[_volumeElements[j].getNegativeAdjacentVolumeElements()[k].getIndex()].getMetricSubspaceElementIndex() == metricSubspaceElementIndex) {
                            buildMetricSubspaceElementsLoop(j, _volumeElements[i].getPositive(), metricSubspaceElementIndex);
                            c++;
                                }
                            }
                        }
                    }
                } while (c > 0);
          
                metricSubspaceElementIndex++;
            }
        }
      
        _metricSubspaceElements.clear();
        _metricSubspaceElements.resize(metricSubspaceElementIndex);
        for(int i = 0; i < (int)_volumeElements.size(); i++) {
            int metricSubspaceElementIndex = _volumeElements[i].getMetricSubspaceElementIndex();
            _metricSubspaceElements[metricSubspaceElementIndex].getVolumeElementIndices().push_back(i);
        }
    }
    int buildMetricSubspaces(int minMetricSubspaceSize) {
        buildMetricSubspaceElements();
        
        _metricSubspaces.clear();
    
        int metricSubspaceIndex = 0;
        
        vector<pair<int, int>> metricSubspaceElementIndicesSizes;
        for(int i = 0; i < (int)_metricSubspaceElements.size(); i++) {
            int elementSubspaceSize = getMetricSubspaceElementSize(i);
            if(elementSubspaceSize >= minMetricSubspaceSize) {
                metricSubspaceElementIndicesSizes.push_back(make_pair(i, elementSubspaceSize));
            }
        }
        
        _metricSubspaces.resize(metricSubspaceElementIndicesSizes.size());
        SubspaceElementIndicesSizesCompare subspaceElementIndicesSizesCompare;
        sort(metricSubspaceElementIndicesSizes.begin(), metricSubspaceElementIndicesSizes.end(), subspaceElementIndicesSizesCompare);
        for(int i = 0; i < (int)metricSubspaceElementIndicesSizes.size(); i++) {
            MetricSubspace volumeElementSubspace;
            volumeElementSubspace.getMetricSubspaceElementIndices().push_back(metricSubspaceElementIndicesSizes[i].first);
            _metricSubspaces[i] = volumeElementSubspace;
          
            _metricSubspaceElements[metricSubspaceElementIndicesSizes[i].first].setMetricSubspaceIndex(metricSubspaceIndex);
          
            metricSubspaceIndex++;
        }
        
        for(int i = 0; i < (int)_metricSubspaceElements.size(); i++) {
            int subspaceElementSize =  getMetricSubspaceElementSize(i);
            if(subspaceElementSize < minMetricSubspaceSize) {
                 set<int> adjacentElementSubspaceIndices = getAdjacentvolumeElementSubspaceElements(i);
    
                if(adjacentElementSubspaceIndices.size() == 0) {
                    continue;
                }
                
                vector<pair<int, int>> metricSubspaceElementIndicesSizes;
                for(set<int>::const_iterator iter =  adjacentElementSubspaceIndices.begin(); iter !=  adjacentElementSubspaceIndices.end(); ++iter) {
                    metricSubspaceElementIndicesSizes.push_back(make_pair(*iter, getMetricSubspaceElementSize(*iter)));
                }
                
                SubspaceElementIndicesSizesCompare elementSubspaceIndicesSizesCompare;
                sort(metricSubspaceElementIndicesSizes.begin(), metricSubspaceElementIndicesSizes.end(), elementSubspaceIndicesSizesCompare);
                
                if(metricSubspaceElementIndicesSizes[0].second < minMetricSubspaceSize) {
                    continue;
                }
                
                int subspaceIndex = _metricSubspaceElements[metricSubspaceElementIndicesSizes[0].first].getMetricSubspaceIndex();
                _metricSubspaces[subspaceIndex].getMetricSubspaceElementIndices().push_back(i);
            }
        }
        
        return _metricSubspaces.size();
    }
    set<int> getAdjacentvolumeElementSubspaceElements(int metricSubspaceElementIndex) {
        set<int> adjacentVolumeElementSubspaceElementIndices;
        vector<int>& volumeElementIndices = _metricSubspaceElements[metricSubspaceElementIndex].getVolumeElementIndices();
        for(int j = 0; j < volumeElementIndices.size(); j++) {
            int volumeElementIndex = volumeElementIndices[j];
            if(_volumeElements[volumeElementIndex].isBoundaryElement()) {
                if(_volumeElements[volumeElementIndex].getPositive()) {
                    for(int k = 0; k < (int)_volumeElements[volumeElementIndex].getNegativeAdjacentVolumeElements().size(); k++) {
                        int adjacentVolumeElementIndex = _volumeElements[volumeElementIndex].getNegativeAdjacentVolumeElements()[k].getIndex();
                        int adjacentElementSubspaceIndex = _volumeElements[adjacentVolumeElementIndex].getMetricSubspaceElementIndex();
                        adjacentVolumeElementSubspaceElementIndices.insert(adjacentElementSubspaceIndex);
                    }
                } else {
                    for(int k = 0; k < (int)_volumeElements[volumeElementIndex].getPositiveAdjacentVolumeElements().size(); k++) {
                        int adjacentVolumeElementIndex = _volumeElements[volumeElementIndex].getPositiveAdjacentVolumeElements()[k].getIndex();
                        int adjacentElementSubspaceIndex = _volumeElements[adjacentVolumeElementIndex].getMetricSubspaceElementIndex();
                        adjacentVolumeElementSubspaceElementIndices.insert(adjacentElementSubspaceIndex);
                    }
                }
            }
        }
    
        return adjacentVolumeElementSubspaceElementIndices;
    }
    int getMetricSubspaceElementSize(int metricSubspaceElementIndex) {
        int metricSubspaceElementSize = 0;
        vector<int>& volumeElementIndices = _metricSubspaceElements[metricSubspaceElementIndex].getVolumeElementIndices();
        for(int i = 0; i < (int)volumeElementIndices.size(); i++) {
            metricSubspaceElementSize += _volumeElements[volumeElementIndices[i]].getGenerativeDataIndices().size();
        }
        return metricSubspaceElementSize;
    }
    int getMetricSubspaceSize(int metricSubspaceIndex) {
        int metricSubspaceSize = 0;
        vector<int>& metricSubspaceElementIndices = _metricSubspaces[metricSubspaceIndex].getMetricSubspaceElementIndices();
        for(int i = 0; i < (int)metricSubspaceElementIndices.size(); i++) {
            metricSubspaceSize += getMetricSubspaceElementSize(metricSubspaceElementIndices[i]);
        }
        return metricSubspaceSize;
    }
    bool getMetricSubspaceElementPositve(int i, bool positive = true) {
        if(_volumeElements[_metricSubspaceElements[i].getVolumeElementIndices()[0]].getPositive() == positive) {
            return true;
        } else {
            return false;
        }
    }
    bool getMetricSubspacePositve(int i, bool positive = true) {
        MetricSubspace& metricSubspace = _metricSubspaces[i];
        int index = metricSubspace.getMetricSubspaceElementIndices()[0];
        return getMetricSubspaceElementPositve(index, positive);
    }
    vector<int> getGenerativeDataVolumeElementIndices() {
        return _generativeDataVolumeElementIndices;
    }
    vector<int> getGenerativeDataVolumeElementIndices(int metricSubspaceIndex, bool boundary = false) {
        vector<int> metricSubspaceGenerativeDataIndices;
        vector<int>& metricSubspaceElementIndices = getMetricSubspaces()[metricSubspaceIndex].getMetricSubspaceElementIndices();
    
        for(int i = 0; i < (int)metricSubspaceElementIndices.size(); i++) {
            int metricSubspaceElementIndex = metricSubspaceElementIndices[i];
            for(int j = 0; j < (int)getMetricSubspaceElements()[metricSubspaceElementIndex].getVolumeElementIndices().size(); j++) {
                int k = getMetricSubspaceElements()[metricSubspaceElementIndex].getVolumeElementIndices()[j]; 
                vector<int> generativeDataIndices = getVolumeElements()[k].getGenerativeDataIndices();
                if(!boundary || (boundary && getVolumeElements()[k].isBoundaryElement())) {
                    metricSubspaceGenerativeDataIndices.insert(metricSubspaceGenerativeDataIndices.end(), generativeDataIndices.begin(), generativeDataIndices.end());
                }
            }
        }
        return metricSubspaceGenerativeDataIndices;
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
        
        size = _metricSubspaceElements.size();
        InOut::Write(os, size);
        for(int i = 0; i < (int)_metricSubspaceElements.size(); i++) {
            _metricSubspaceElements[i].write(os);
        }
        
        size = _metricSubspaces.size();
        InOut::Write(os, size);
        for(int i = 0; i < (int)_metricSubspaces.size(); i++) {
            _metricSubspaces[i].write(os);
        }
        
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
        
        size = _metricSubspaceElements.size();
        InOut::Read(is, size);
        _metricSubspaceElements.resize(size);
        for(int i = 0; i < (int)_metricSubspaceElements.size(); i++) {
            _metricSubspaceElements[i].read(is);
        }
        
        size = _metricSubspaces.size();
        InOut::Read(is, size);
        _metricSubspaces.resize(size);
        for(int i = 0; i < (int)_metricSubspaces.size(); i++) {
            _metricSubspaces[i].read(is);
        }
        
        InOut::Read(is, _generativeDataVolumeElementIndices);
    }
    
private:
    float _level;
    vector<VolumeElement> _volumeElements;
    
    VolumeElementConfigurationMap _volumeElementConfigurationMap;
    VpTree<bool>* _pVpTree;
    L1Distance<bool> _l1Distance;
    VpVolumeElementConfigurations<bool>* _pVpVolumeElementConfigurations;
    
    vector<MetricSubspaceElement> _metricSubspaceElements;
    vector<MetricSubspace> _metricSubspaces;
    
    vector<int> _generativeDataVolumeElementIndices;
};

#endif
