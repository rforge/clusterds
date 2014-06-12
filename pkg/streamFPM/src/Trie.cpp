#include <iostream>
#include <vector>
#include <math.h>
#include <algorithm>
#include "Trie.h"

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

bool Trie::updateWord(std::vector<int> & itemset, int k, int d, int minsup, int dk, int len, int first, Node* current)
{

    if(current == NULL)
        current = root;
    else {
      
        current->setCount( (current->getCount() * pow(d, k - current->getId())) + 1);
        current->setErr(current->getErr() * pow(d, k - current->getId()));
        
        current->setId(k);
    }

    for ( int i = first; i < itemset.size(); i++ )
    {
        Node* tmp = current->findChild(itemset[i]);
        if ( tmp == NULL )
        {
          //FIXME: might change this to the delayed add ?
            std::cout << "itemset not in tree. must add to tree before updating" <<std::endl;
            delayedInsertion(itemset, k, d, minsup,  dk, len, i, current);
            //return false;
        }
        //FIXME minsup should be pruning support
        //FIXME  bad stuff with doubles
        else if(current->getCount()/dk < minsup * 0.8  && len > 1) {
            deleteNodeAndChildren(current);
        }
        else {
            updateWord(itemset, k, d, minsup,  dk, len + 1, i+1, tmp);
        }
    }

    return true;
}

bool Trie::delayedInsertion(std::vector<int> & itemset, int k, int d, int minsup, int dk, int len, int first, Node* current)
{ 
  /*
    if(len == 0){
       Node* tmp = new Node();
       tmp->setContent(itemset[first]);
       tmp->setCount(1);
       tmp->setErr(0);
       tmp->setId(k);
       current->appendChild(tmp);
    }
    else {
      int cmax = min(Pn(e)) //Pn(e) is counts for all subsets of e of size n-1
      Estimate Cmax(e) and Cmin(e)
      if(Cmax(e) > Cupper(e))
        Cmax(e) <- Cupper(e)
      if(Cmax(e)/dk  >= Sins){
        rt$addSet(e, err=Cmax(e)-Cmin(e), tid=k)
      }
        
    }
  
  */
  return false; //remove this
}

/**
 * should be called with current = root and depth = 0
 */
/*
int Trie::Cmax(std::vector<int> & itemset, Node * current, int depth = 0, int start = 0)
{
  int maxDepth = itemset.size() - 1;
  int min = -1;
  
  if (depth == maxDepth) {
      return current.getCount();
  }
  
  if (current == root) {
    depth = 0;
    for (int i = 0; i < 2; i ++) {
      Node* tmp = current->findChild(itemset[i]);
      min = Cmax(itemset, tmp, depth + 1, i+1);
    }
  }
  else if(current != NULL && depth < maxDepth) {
    Node* tmp = current->findChild(itemset[i]);
    int count = min(Cmax(itemset, tmp, depth + 1, i+1), count);
    for (int i = start; i < itemset.size()){
      Node* tmp = current->findChild(itemset[i]);
      count = min(Cmax(itemset, tmp, depth + 1, i+1), count);
    }

  }
  return 0;
  
}*/