// Copyright 2021 Werner Mueller
// Released under the GPL (>= 2)

#ifndef VP_TREE
#define VP_TREE

#include <queue>
#include <random>
#include <cmath>

#include "inOut.h"
#include "progress.h"
#include "generativeData.h"

#define GD_RCPP

#ifdef GD_RCPP
#include <Rcpp.h>
#endif

using namespace std;

const string cDifferentSizes = "Sizes of vectors are different";
const string cNearestNeighborDifferent = "Nearest neighbor is different";
int cMaxNearestNeighbors = 256;

template <typename T>
struct LpDistance{
    LpDistance() {}
    virtual ~LpDistance() {
    }
    virtual float operator()(const vector<T>& a, const vector<T>& b) = 0;
};

template <typename T>
struct L1Distance : public LpDistance<T> {
    float operator()(const vector<T>& a, const vector<T>& b) {
        if(a.size() != b.size()) {
            throw string(cDifferentSizes);
        }
        float d = 0.0;
        for(int i = 0; i < a.size(); i++) {
            d += abs(a[i] - b[i]);
        }
        return d;
    }
};

template <typename T>
struct L2Distance : public LpDistance<T> {
    float operator()(const vector<T>& a, const vector<T>& b) {
        if(a.size() != b.size()) {
            throw string(cDifferentSizes);
        }
        float d = 0.0;
        for(int i = 0; i < a.size(); i++) {
            d += (a[i] - b[i]) * (a[i] - b[i]);
        }
        return sqrt(d);
    }
};

template <typename T>
struct L2DistanceNan : public LpDistance<T> {
    float operator()(const vector<T>& a, const vector<T>& b) {
        if(a.size() != b.size()) {
            throw string(cDifferentSizes);
        }
        float d = 0.0;
        for(int i = 0; i < a.size(); i++) {
            if(isnan(a[i]) || isnan(b[i])) {
                continue;
            }
            d += (a[i] - b[i]) * (a[i] - b[i]);
        }
        return sqrt(d);
    }
};

template <typename T>
struct L2DistanceNanIndexed : public LpDistance<T> {
    L2DistanceNanIndexed(const vector<float>& distance): _distance(distance) {
    }
    L2DistanceNanIndexed(const L2DistanceNanIndexed& l2DistanceNanIndexed): _distance(l2DistanceNanIndexed._distance) {
    }
    float operator()(const vector<T>& a, const vector<T>& b) {
        //Function f("message");
        //f("indexed");
        //f(a.size());
        //f(b.size());
        if(a.size() != _distance.size() || b.size() != _distance.size()) {
            throw string(cDifferentSizes);
        }
        float d = 0.0;
        for(int i = 0; i < a.size(); i++) {
            if(isnan(_distance[i])) {
                continue;
            }
            d += (a[i] - b[i]) * (a[i] - b[i]);
        }
        return sqrt(d);
    }
    vector<float> _distance;
};

template <>
struct L1Distance<bool> : public LpDistance<bool> {
    float operator()(const vector<bool>& a, const vector<bool>& b) {
        if(a.size() != b.size()) {
            throw string(cDifferentSizes);
        }
        float d = 0.0;
        for(int i = 0; i < a.size(); i++) {
            if(a[i] != b[i])
            {
                d += 1.0;
            }
        }
        return d;
    }
};

template <typename T>
class VpTreeData {
public:
    VpTreeData() {
    }
    virtual ~VpTreeData() {
    }
    
    virtual vector<float> getNumberVector(int i) = 0;
    virtual vector<T>& getReferenceNumberVector(int i) = 0;
    virtual int getSize() = 0;
};

template <typename T>
class VpGenerativeData : public VpTreeData<T> {
public:
    VpGenerativeData(GenerativeData& generativeData): _pGenerativeData(&generativeData) {}
    VpGenerativeData(const VpGenerativeData& vpGenerativeData): _pGenerativeData(vpGenerativeData._generativeData) {
    }
    virtual VpTreeData<T>& assign(const VpTreeData<T>& vpTreeData) {
        *this = dynamic_cast<const VpGenerativeData&>(vpTreeData);
        return *this;
    }
    
    virtual vector<float> getNumberVector(int i) {
        return _pGenerativeData->getNormalizedNumberVector(i);
    }
    virtual vector<T>& getReferenceNumberVector(int i) {
        return  _pGenerativeData->getNormalizedNumberVectorReference(i);
    }
    virtual int getSize() {
        return _pGenerativeData->getNormalizedSize();
    }
    
private:
    GenerativeData* _pGenerativeData;
};

template <typename T>
class VpIndexGenerativeData : public VpTreeData<T> {
public:
    VpIndexGenerativeData(GenerativeData& generativeData, vector<int>& indexVector): _generativeData(generativeData), _indexVector(indexVector) {}
    
    virtual vector<float> getNumberVector(int i) {
        return  _generativeData.getNormalizedNumberVector(_indexVector[i]);
    }
    virtual vector<T>& getReferenceNumberVector(int i) {
        return _generativeData.getNormalizedNumberVectorReference(_indexVector[i]);
    }
    virtual int getSize() {
        return _indexVector.size();
    }
    
private:
    GenerativeData& _generativeData;
    vector<int> _indexVector;
};

template <typename T>
struct Distance {
    Distance(VpTreeData<T>& vpTreeData, LpDistance<T>& lpDistance): _vpTreeData(vpTreeData), _lpDistance(lpDistance) {}
    float operator()(const int& a, const int& b) {
        vector<T>& aNumberVector = _vpTreeData.getReferenceNumberVector(a);
        vector<T>& bNumberVector = _vpTreeData.getReferenceNumberVector(b);
        return _lpDistance(aNumberVector, bNumberVector);
    }
  
    VpTreeData<T>& _vpTreeData;
    LpDistance<T>& _lpDistance;
};

template <typename T>
struct VpDistance {
    VpDistance(VpTreeData<T>& vpTreeData, int index, LpDistance<T>& lpDistance): _vpTreeData(vpTreeData), _index(index), _lpDistance(lpDistance) {}
    bool operator()(const int& a, const int& b) {
        vector<T>& aNumberVector = _vpTreeData.getReferenceNumberVector(a);
        vector<T>& bNumberVector = _vpTreeData.getReferenceNumberVector(b);
        vector<T>& cNumberVector = _vpTreeData.getReferenceNumberVector(_index);
        if(_lpDistance(aNumberVector, cNumberVector) < _lpDistance(bNumberVector, cNumberVector)) {
            return true;
        } else {
            return false;
        }
    }

    VpTreeData<T>& _vpTreeData;
    int _index;
    LpDistance<T>& _lpDistance;
};

class VpElement {
public:
    VpElement(): _index(-1), _distance(0) {
        ;
    }
    VpElement(int index, float distance): _index(index), _distance(distance) {
        ;
    }
    VpElement(int index, float distance, bool positive): _index(index), _distance(distance) {
        ;
    }
    
    int getIndex() const {
        return _index;
    }
    void setIndex(int index) {
        _index = index;
    }
    float getDistance() const {
        return _distance;
    }
    void setDistance(float distance) {
        _distance = distance;
    }
    bool operator<(const VpElement& vpElement) const {
        return getDistance() < vpElement.getDistance();
    }
    
    void write(ofstream& os) {
        InOut::Write(os, _index);
        InOut::Write(os, _distance);
    }
    void read(ifstream& is) {
        InOut::Read(is, _index);
        InOut::Read(is, _distance);
    }
    
private:
    int _index;
    float _distance;
};

struct VpElementCompare{
    bool operator()(const VpElement& a, const VpElement& b) {
        if(a.getDistance() < b.getDistance()) {
            return true;
        }
        if(a.getDistance() == b.getDistance()) {
            if(a.getIndex() < b.getIndex()) {
                return true;
            }
        }
        return false;
    }
};

class VpNode {
public:
	VpNode(): _index(-1), _threshold(0), _pInVpNode(0), _pOutVpNode(0) {}
    ~VpNode() {
        delete _pInVpNode;
        delete _pOutVpNode;
    }
	
    int getIndex() {
        return _index;
    }
    void setIndex(int index) {
        _index = index;
    }
    float getThreshold() {
        return _threshold;
	}
    void setThreshold(float threshold) {
        _threshold = threshold;
    }
	VpNode* getInVpNode() {
		return _pInVpNode;
	}
    void setInVpNode(VpNode* pVpNode) {
        _pInVpNode = pVpNode;
    }
    VpNode* getOutVpNode() {
        return _pOutVpNode;
    }
    void setOutVpNode(VpNode* pVpNode) {
        _pOutVpNode = pVpNode;
    }
  
private:
    int _index;
    float _threshold;
	VpNode* _pInVpNode;
	VpNode* _pOutVpNode;
};

template <typename T>
class VpTree {
public:
    VpTree(): _pVpNode(0), _pVpTreeData(0), _tau(numeric_limits<float>::max()), _pProgress(0), _pLpDistance(0), _pG(new mt19937(_rd())), _pR(0), _pGd(0) {
    }
    VpTree(VpTreeData<T>* pVpTreeData, LpDistance<T>* pLpDistance, Progress* pProgress): _pVpNode(0), _pVpTreeData(pVpTreeData), _tau(numeric_limits<float>::max()), _pProgress(pProgress), _pLpDistance(pLpDistance), _pG(new mt19937(_rd())), _pR(0), _pGd(0) {
    }
    ~VpTree() {
        delete _pVpNode;
        delete _pR;
        delete _pG;
        delete _pGd;
    }
    
    VpNode* build(int lower, int upper) {
        if(_pProgress != 0) {
            (*_pProgress)(_i);
        }
        
        if(upper == lower) {
            return 0;
        }
    
        VpNode* pVpNode = new VpNode();
        pVpNode->setIndex(lower);
    
        if(upper - lower > 1) {
            //int i = (int)((float)(*_pR)(*_pGd) / (float)(_pVpTreeData->getSize() - 1) * (float)(upper - lower - 1)) + lower;
            uniform_int_distribution<int> uid(lower, upper - 1);
            int i = uid(*_pGd);
            
            swap(_indexVector[lower], _indexVector[i]);
            int median = (upper + lower) / 2;
            nth_element(_indexVector.begin() + lower + 1,
                 _indexVector.begin() + median,
                _indexVector.begin() + upper,
                VpDistance<T>(*_pVpTreeData, _indexVector[lower], *_pLpDistance));
          
            pVpNode->setThreshold(Distance<T>(*_pVpTreeData, *_pLpDistance).operator()(_indexVector[lower], _indexVector[median]));
            pVpNode->setIndex(lower);
            pVpNode->setInVpNode(build(lower + 1, median));
            pVpNode->setOutVpNode(build(median, upper));
        }
        _i++;
    
        return pVpNode;
    }
    void build(VpTreeData<T>* pVpTreeData, LpDistance<T>* pLpDistance, Progress* pProgress) {
        delete _pVpNode;
        
        _pVpTreeData = pVpTreeData;
        _pLpDistance = pLpDistance;
        _pProgress = pProgress;
        _i = 0;
        
        _indexVector.resize(_pVpTreeData->getSize());
        for(int i = 0; i < _pVpTreeData->getSize(); i++) {
            _indexVector[i] = i;
        }
        delete _pGd;
        //_pGd = new default_random_engine(30);
        _pGd = new default_random_engine(1);
        delete _pR;
        _pR = new uniform_int_distribution<int>(0, _pVpTreeData->getSize() - 1);
        _pVpNode = build(0, _indexVector.size());
        
        if(_pProgress != 0) {
            (*_pProgress)(_pVpTreeData->getSize());
        }
    }
    bool isBuilt() {
        if(_pVpNode != 0) {
            return true;
        } else {
            return false;
        }
    }
  
    void search(const vector<T>& target, int kDistances, int k, vector<VpElement>& nearestNeighbors) {
        priority_queue<VpElement> priorityQueue;
        _tau = numeric_limits<float>::max();
        _unique.clear();
        search(_pVpNode, target, kDistances, k, priorityQueue);
        
        nearestNeighbors.clear();
        while(!priorityQueue.empty()) {
            nearestNeighbors.push_back(priorityQueue.top());
            priorityQueue.pop();
        }
        reverse(nearestNeighbors.begin(), nearestNeighbors.end());
    }

    void search(VpNode* pVpNode, const vector<T>& target, int kDistances, int k, priority_queue<VpElement>& priorityQueue) {
        if(pVpNode == 0) {
            return;
        }
        
        vector<T>& numberVector = _pVpTreeData->getReferenceNumberVector(_indexVector[pVpNode->getIndex()]);
        float d = (*_pLpDistance)(numberVector, target);
        if(d <= _tau) {
            _unique.insert(d);
            if(_unique.size() > kDistances || priorityQueue.size() > k) {
                float tau = priorityQueue.top().getDistance();
                while(!priorityQueue.empty() && priorityQueue.top().getDistance() == tau) {
                    priorityQueue.pop();
                }
                _unique.erase(tau);
                priorityQueue.push(VpElement(_indexVector[pVpNode->getIndex()], d));   
                _tau = priorityQueue.top().getDistance();
            } else {
                priorityQueue.push(VpElement(_indexVector[pVpNode->getIndex()], d));
            }
        }
        
        if(d < pVpNode->getThreshold()) {
            search(pVpNode->getInVpNode(), target, kDistances, k, priorityQueue);
            if(d + _tau >= pVpNode->getThreshold()) {
                search(pVpNode->getOutVpNode(), target, kDistances, k, priorityQueue);
            }
        } else if(d == pVpNode->getThreshold()) {
            search(pVpNode->getInVpNode(), target, kDistances, k, priorityQueue);
            search(pVpNode->getOutVpNode(), target, kDistances, k, priorityQueue);
        } else if(d > pVpNode->getThreshold()) {
            search(pVpNode->getOutVpNode(), target, kDistances, k, priorityQueue);
            if(d - _tau <= pVpNode->getThreshold()) {
                search(pVpNode->getInVpNode(), target, kDistances, k, priorityQueue);
            }
        }
    }
    void linearSearch(const vector<T>& target, int kDistances, int k, vector<VpElement>& nearestNeighbors) {
        priority_queue<VpElement> priorityQueue;
        float tau = numeric_limits<float>::max();
        _unique.clear();
        
        for(int i = 0; (int)i < _pVpTreeData->getSize(); i++) {
            vector<T>& numberVector = _pVpTreeData->getReferenceNumberVector(i);
            float d = (*_pLpDistance)(numberVector, target);
            if(d <= _tau) {
                _unique.insert(d);
                if(_unique.size() > kDistances || priorityQueue.size() > k) {
                    float tau = priorityQueue.top().getDistance();
                    while(!priorityQueue.empty() && priorityQueue.top().getDistance() == tau) {
                        priorityQueue.pop();
                    }
                    _unique.erase(tau);
                    priorityQueue.push(VpElement(i, d));  
                    _tau = priorityQueue.top().getDistance();
                    
                } else {
                    priorityQueue.push(VpElement(i, d));
                }
            }
        }
        
        nearestNeighbors.clear();
        while(!priorityQueue.empty()) {
            nearestNeighbors.push_back(priorityQueue.top());
            priorityQueue.pop();
        }
        reverse(nearestNeighbors.begin(), nearestNeighbors.end());
    }
   
    VpTreeData<T>& getVpTreeData() {
        return *_pVpTreeData;
    }
    LpDistance<T>& getLpDistance() {
        return *_pLpDistance;
    }
    
private:
    vector<int> _indexVector;
    VpNode* _pVpNode;
    VpTreeData<T>* _pVpTreeData;
    float _tau;
    Progress* _pProgress;
    LpDistance<T>* _pLpDistance;
    
    random_device _rd;
    mt19937* _pG;
    uniform_int_distribution<int>* _pR;
    
    set<float> _unique;
    int _i;
    
    default_random_engine *_pGd;
};

#endif
