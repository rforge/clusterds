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
    bool updateWord(std::vector<int> & itemset, int k, int d, int minsup, int dk, int len, int first, Node* current = NULL);
    bool delayedInsertion(std::vector<int> & itemset, int k, int d, int minsup, int dk, int len, int first, Node* current);
    
private:
    int deleteWordRecursion(std::vector<int> itemset, int i, Node * current);
    int Cmax(std::vector<int> & itemset);
    Node* root;
};
