#include <iostream>
#include <vector>
#include "Node.h"

class Trie {
public:
    Trie();
    ~Trie();
    void addItemsetDirectly(std::vector<int> itemset);
    bool searchItemset(std::vector<int> itemset);
    bool deleteItemset(std::vector<int> itemset);
    bool deleteNodeAndChildren(Node* current);
    void deleteDescendentNodes(Node* current);
    bool deleteChildNodeAndDescendents(Node* parent, int content);
    bool updateWord(std::vector<int> & itemset, int k, double d, double minsup, double dk, int len, int first);
    bool delayedInsertion(std::vector<int> & itemset, int k, double d, double minsup, double dk, int startIndex, int endIndex, Node* current);
    bool insertionPhase(std::vector<int> & itemset, int k, double d, double minsup, double dk, int len, int first);
    void printTree(Node * current);
    std::vector<std::vector<int> > getMostFrequentItemset();
    
      
private:

    Node* root;
    
    double totalSetsWithDecay;
    int lastTID;
    void getMostFrequentItemset(Node * current, std::vector<std::vector<int> > &freqItems, std::vector<int> &counts, std::vector<int> &currentSet, int depth);
    bool insertionPhase(std::vector<int> & itemset, int k, double d, double minsup, double dk, int len, int first, Node* current);
    bool updateWordRecursion(std::vector<int> & itemset, int k, double d, double minsup, double dk, int len, int first, Node* current = NULL);
    int deleteWordRecursion(std::vector<int> itemset, int i, Node * current);
    int cMax(std::vector<int> & itemset, int startIndex, int endIndex);
    int cMin(std::vector<int> & itemset, int startIndex, int endIndex, double dk);
    double cUpper(std::vector<int> & itemset, double minsup, double dk, double d);
    int getCountForSubset(std::vector<int>& itemset, int startIndex, int endIndex, int excludedIndex1 = -1, int excludedIndex2 = -1);
    
};