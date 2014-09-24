#include <iostream>
#include <vector>
#include "Node.h"

class Trie {
public:
    Trie();
    ~Trie();
    void addWord(std::vector<int> itemset);
    bool searchWord(std::vector<int> itemset);
    bool deleteWord(std::vector<int> itemset);
    bool deleteNodeAndChildren(Node* current);
    bool updateWord(std::vector<int> & itemset, int k, double d, double minsup, double dk, int len, int first);
    bool delayedInsertion(std::vector<int> & itemset, int k, double d, double minsup, double dk, int len, int first, Node* current);
    bool insertionPhase(std::vector<int> & itemset, int k, double d, double minsup, double dk, int len, int first);
    void printTree(Node * current);
    void getMostFrequentItemset();
    
    
private:
    bool insertionPhase(std::vector<int> & itemset, int k, double d, double minsup, double dk, int len, int first, Node* current);
    bool updateWordRecursion(std::vector<int> & itemset, int k, double d, double minsup, double dk, int len, int first, Node* current = NULL);
    int deleteWordRecursion(std::vector<int> itemset, int i, Node * current);
    int cMax(std::vector<int> & itemset, Node * current, int depth = 0, int start = 0);
    int cMin(std::vector<int> & itemset);
    double cUpper(std::vector<int> & itemset, double minsup, double dk, double d);
    Node* root;
};