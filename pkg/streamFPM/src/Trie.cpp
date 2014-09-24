#include <iostream>
#include <vector>
#include <math.h>
#include <algorithm>
#include "Trie.h"

using namespace std;

//NOTE: THIS IS THE TRIE FOR EST DEC

Trie::Trie()
{
    root = new Node();
}

Trie::~Trie()
{
    // Free memory
}

void Trie::addWord(std::vector<int> itemset)
{
    std::sort(itemset.begin(), itemset.end());
    Node* current = root;

    if ( itemset.size() == 0 )
    {
        return;
    }

    for ( int i = 0; i < itemset.size(); i++ )
    {    
        Node* child = current->findChild(itemset.at(i));

        if ( child != NULL )
        {   
            child->incrementCount(1);
            //FIXME: also set TID
            current = child;
        }
        else
        {
            Node* tmp = new Node();
            tmp->setContent(itemset[i]);
            tmp->setCount(1);
            //FIXME: also set TID
            current->appendChild(tmp);
            current = tmp;
        }
        //if ( i == itemset.size() - 1 )
        //    current->setWordMarker();

        std::cout << current->content() <<std::endl;
    }
}


bool Trie::searchWord(std::vector<int> itemset)
{
    std::sort (itemset.begin(), itemset.end());
    Node* current = root;

    while ( current != NULL )
    {
        for ( int i = 0; i < itemset.size(); i++ )
        {
            Node* tmp = current->findChild(itemset[i]);
            if ( tmp == NULL )
                return false;
            current = tmp;
        }

        return true;
    }

    return false;
}



//FIXME ALL oF THIS
bool Trie::deleteWord(std::vector<int> itemset)
{
    std::sort(itemset.begin(), itemset.end());

    Node* current = root;
    int i = 0;
    
    if (itemset.size() == 0) {
      std::cout << "Itemset is empty. Cannot remove" << std::endl;
      return false;
    }
    
    Node* tmp = current->findChild(itemset[i]);

    if (tmp == NULL){
      std::cout << "Itemset does not exist" << std::endl;
      return false;
    }
      
    int ret = deleteWordRecursion(itemset, i+1, tmp);

    if(ret == 1) {
      current->deleteChild(itemset[i]);
      return true;
    }
    
    return false;
    
}

int Trie::deleteWordRecursion(std::vector<int> itemset, int i, Node * current) 
{
  std::cout << "r1" << std::endl;
  Node* tmp = NULL;
  if(i < itemset.size()) {
    tmp = current->findChild(itemset[i]);
  }
    
  if(tmp == NULL || i >= itemset.size()) {
    std::cout << "r2" << std::endl;
    if(current->children().size() == 0) {
      return 1;
    }
    else{
      if(current->wordMarker())
        current->removeWordMarker();
        return 2;
    }
  }
  
  else {
    int temp = deleteWordRecursion(itemset, i+1, tmp);
    
    if(temp == 1) {
      current->deleteChild(itemset[i]);
      if(current->children().size() == 0 && current->wordMarker() == false)
        return 1;
    }
    else if(temp == 2) {
      return 2;
    }
    
  }
  
  
}

//FIXME
bool Trie::deleteNodeAndChildren(Node* current)
{
  return false;
}

bool Trie::updateWord(std::vector<int> & itemset, int k, double d, double minsup, double dk, int len, int first)
{
  //calls with current = root
  updateWordRecursion(itemset, k, d, minsup, dk, len, first, root);
}

bool Trie::updateWordRecursion(std::vector<int> & itemset, int k, double d, double minsup, double dk, int len, int first, Node* current)
{
  
    if (current != root)
    {
      std::cout << "here2 update set" << std::endl;
      current->setCount( (current->getCount() * pow(d, k - current->getId())) + 1);
      current->setErr(current->getErr() * pow(d, k - current->getId()));
      current->setId(k);
    }
    
    for ( int i = first; i < itemset.size(); i++ )
    {
        std::cout << "set: " << i << " starting with item:" << itemset[i] <<endl;
        Node* tmp = current->findChild(itemset[i]);
        //FIXME: FIX LEN
        
        if ( tmp == NULL )
        {
            //do nothing
        }
        //FIXME minsup should be pruning support
        //FIXME  bad stuff with doubles
        //should be tmp?
        else if(current->getCount()/dk < minsup * 0.8  && len > 1) {
            std::cout << "remove node" << std::endl;
            deleteNodeAndChildren(current);
        }
        else {
            std::cout << "calling recursion" << std::endl;
            updateWordRecursion(itemset, k, d, minsup,  dk, len + 1, i+1, tmp);
        }
        
    }

    return true;
}

bool Trie::insertionPhase(std::vector<int> & itemset, int k, double d, double minsup, double dk, int len, int first)
{
    insertionPhase(itemset, k, d, minsup, dk, len, first, root);
}

bool Trie::insertionPhase(std::vector<int> & itemset, int k, double d, double minsup, double dk, int len, int first, Node* current)
{
    for ( int i = first; i < itemset.size(); i++ )
    {
        Node* tmp = current->findChild(itemset[i]);
        if ( tmp == NULL )
        {
            std::cout << "here7" << std::endl;
            delayedInsertion(itemset, k, d, minsup,  dk, len, i, current);
            Node* tmp1 = current->findChild(itemset[i]);
            insertionPhase(itemset, k, d, minsup,  dk, len + 1, i+1, tmp1);
        }
        else {
          std::cout << "calling recursion" << std::endl;
           insertionPhase(itemset, k, d, minsup,  dk, len + 1, i+1, tmp);
        }

    }
}

bool Trie::delayedInsertion(std::vector<int> & itemset, int k, double d, double minsup, double dk, int len, int first, Node* current)
{ 
    //FIXME: SHOULD BE == 0, but cmax or cmin doesnt work/ is causing crash i think
    if (len >= 0)
    {
       std::cout << "added item: "<< itemset[first] << " to " << current->content()  << std::endl;
       Node* tmp = new Node();
       tmp->setContent(itemset[first]);
       tmp->setCount(1);
       tmp->setErr(0);
       tmp->setId(k);
       current->appendChild(tmp);
    }
    else 
    {
      std::cout << "here5 add new set" << std::endl;
      int cmax = cMax(itemset, root, 0, 0);
      int cmin = cMin(itemset);
      if(cmax > cUpper(itemset, minsup, dk, d))
        cmax = cUpper(itemset, minsup, dk, d);
      //FIXME bad double stuff
      if(cmax/dk  >= minsup * 0.8)
      {
        Node* tmp = new Node();
        tmp->setContent(itemset[first]);
        tmp->setCount(cmax);
        tmp->setErr(cmax-cmin);
        tmp->setId(k);
        current->appendChild(tmp);
        //rt$addSet(e, err= cmax - cmin, tid=k)
      }
        
    }
    
    return false;
}

/**
 * should be called with current = root and depth = 0
 */                                                 //     default to 0 and 0
int Trie::cMax(std::vector<int> & itemset, Node * current, int depth, int start)
{
  int maxDepth = itemset.size() - 1;
  
  if (depth == maxDepth) {
      return current->getCount();
  }
  
  if (current == root) {
    depth = 0;
    //why only doing this for itemset[0] and itemset[1] FIXME
    int minimum = min(cMax(itemset, current->findChild(itemset[0]), depth+1, 1), cMax(itemset, current->findChild(itemset[1]), depth+1, 2));
    return minimum;
  }
  else if(current != NULL && depth < maxDepth) {
    
    Node* tmp = current->findChild(itemset[start]);
    int count = min(cMax(itemset, tmp, depth + 1, start+1), count);
    
    for (int i = start+1; i < itemset.size(); i++){
      tmp = current->findChild(itemset[i]);
      count = min(cMax(itemset, tmp, depth + 1, i+1), count);
    }
    
    return count;

  }
  return 0;
  
}

//cmin(e) = max({ cmin(ai U aj) | all ai, aj in Pn-1(e) and i != j })

int Trie::cMin(std::vector<int> & itemset)
{
  //FIXME implement this
  int maximum = 0;
  
  for (int i = 0; i < itemset.size(); i++ ) {
    for (int j = 0; j < itemset.size(); j++) {

      if(i != j) {
        int count1 = 0;
        int count2 = 0;
        int count3 = 0;
        Node * current1 = root;
        Node * current2 = root;
        Node * current3 = root;
        for (int x = 0; x < itemset.size(); x++) {
          
          if(x != i && current1 != NULL){
             current1 = current1->findChild(itemset[x]);
             if(current1 != NULL)
               count1 = current1->getCount();
          }
          if(x != j && current2 != NULL){
             current2 = current2->findChild(itemset[x]);
             if(current2 != NULL)
               count2 = current2->getCount();            
          }
          if(x != i && x != j && current3 != NULL){
             current3 = current3->findChild(itemset[x]);
             if(current3 != NULL)
               count3 = current3->getCount();            
          }
        }
        
        maximum = max(maximum, count1 + count2 - count3);
      }
      
    } //end j for loop
  } //end i for loop
  
  return maximum;
}

double Trie::cUpper(std::vector<int> & itemset, double minsup, double dk, double d)
{
  
  //return maxCntBeforeSubsets + cntForSubsets
  double maxCntBeforeSubsets = (1 - pow(d,(itemset.size() -1 )) ) / (1-d);
  double cntForSubsets = minsup * dk - (itemset.size() - 1) *  pow(d,(itemset.size() -1 ));
  return maxCntBeforeSubsets + cntForSubsets;
}

//default is root
void Trie::printTree(Node * current)
{
  if(current == NULL)
    current = root;
  vector<Node*> c = current->children();
  for (int i = 0; i < c.size(); i++) {
    printTree(c[i]);
  }
  cout << current->content() << " : count :" << current->getCount() << endl;
}

void Trie::getMostFrequentItemset()
{
  
}