#include <iostream>
#include <vector>
#include <list>
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

        //std::cout << current->content() <<std::endl;
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
      //std::cout << "Itemset is empty. Cannot remove" << std::endl;
      return false;
    }
    
    Node* tmp = current->findChild(itemset[i]);

    if (tmp == NULL){
      //std::cout << "Itemset does not exist" << std::endl;
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

//FIXME: implement
bool Trie::deleteNodeAndChildren(Node* current)
{
  return false;
}


/**
 * Calls updateWordRecusion with current = root, rince Rt class doesn't have access to root node
 * 
 */
bool Trie::updateWord(std::vector<int> & itemset, int k, double d, double minsup, double dk, int len, int first)
{
  //calls with current = root
  updateWordRecursion(itemset, k, d, minsup, dk, len, first, root);
}


/**
 * This method recursively updates the counts/estimates of the tree for the new itemset, itemset;
 * also deletes nodes that dont meet the min support
 * 
 * std::vector<int> itemset: complete itemset that we are updating the tree with
 * int k:
 * double d:
 * double minsup:
 * double dk:
 * int len: the depth of the current node, root depth = 0
 * int first: 
 * Node* current:
 */
bool Trie::updateWordRecursion(std::vector<int> & itemset, int k, double d, double minsup, double dk, int len, int first, Node* current)
{
    if (current != root)
    {
      if(len == 1)
      {
        //std::cout << "updating 1 itemset: " << current->content() << std::endl;
        current->setCount( current->getCount() + 1);
        current->setErr(0);
        current->setId(k);
      }
      else
      {
        //std::cout << "here2 update set: " << current->content() << std::endl;
        current->setCount( (current->getCount() * pow(d, k - current->getId())) + 1);
        current->setErr(current->getErr() * pow(d, k - current->getId()));
        current->setId(k);
      }

    }
    
    
    for ( int i = first; i < itemset.size(); i++ )
    {
        //std::cout << "set: " << i << " starting with item:" << itemset[i] <<endl;
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
            //FIXME: implement deleteNodeAndChild
            deleteNodeAndChildren(current);
        }
        else {
            //std::cout << "calling recursion" << std::endl;
            updateWordRecursion(itemset, k, d, minsup,  dk, len + 1, i+1, tmp);
        }
        
    }

    return true;
}


//calls insertionPhase with current = root, since RT doesn't have access to root
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
            //std::cout << "here7" << std::endl;
            bool successfullyInserted = false;
            successfullyInserted = delayedInsertion(itemset, k, d, minsup,  dk, first, i, current); //change to i, first from len and i
            
            if(successfullyInserted)
            {
              Node* tmp1 = current->findChild(itemset[i]);
              if(tmp1 != NULL)
                insertionPhase(itemset, k, d, minsup,  dk, len + 1, i+1, tmp1);
            }
            
        }
        else {
           //std::cout << "calling recursion" << std::endl;
           insertionPhase(itemset, k, d, minsup,  dk, len + 1, i+1, tmp);
        }

    }
}


bool Trie::delayedInsertion(std::vector<int> & itemset, int k, double d, double minsup, double dk, int startIndex, int endIndex, Node* current)
{ 
    //if current == root, len == 0 i.e. if it is an uninserted 1-itemset
    if (endIndex - startIndex == 0) //FIXME should be ==
    {
       //std::cout << "added item: "<< itemset[startIndex] << " to " << current->content()  << std::endl;
       Node* tmp = new Node();
       tmp->setContent(itemset[startIndex]);
       tmp->setCount(1);
       tmp->setErr(0);
       tmp->setId(k);
       current->appendChild(tmp);
       
    }
    else 
    {
      //std::cout << "here5 add new set" << std::endl;
      
      //FIXME: these should not be called for the whole itemset, but for the subset we are inserting
      
      //FIXME: make all these return double
      int cmax_int = cMax(itemset, startIndex, endIndex);
      double cmax_double = (double) cmax_int;
      //std::cout << "cmax done" << std::endl;
      int cmin = cMin(itemset, startIndex, endIndex, dk);
      //std::cout << "cmin done" << std::endl;
      double cupper = cUpper(itemset, minsup, dk, d); //FIXME to match others
      //std::cout << "cupper done" << std::endl;

      if(cmax_double > cupper)
        cmax_double = cupper;
        
      //std::cout << "here6 calculated values" << std::endl;
    
      
      if(cmax_double/dk  >= minsup * 0.8)
      {
        Node* tmp = new Node();
        tmp->setContent(itemset[endIndex]);
        tmp->setCount(cmax_int);
        tmp->setErr(cmax_int - cmin);
        tmp->setId(k);
        current->appendChild(tmp);
        //addSet(e, err= cmax - cmin, tid=k)
        
        //std::cout << "added item past minsup: "<< itemset[endIndex] << " to " << current->content()  << std::endl;
        
        return true;
      }
        
      return false;
        
    }
    
    return false;
}

/**
 * finds the minimum count of an itemsets subsets, given a larger itemset and a starting and ending index
 * itemset: a full itemset
 * startIndex: first position of the itemset to consider
 * endIndex: last position of the itemset to consider
 * 
 */                                                
int Trie::cMax(std::vector<int> & itemset, int startIndex, int endIndex)
{
  //FIXME: wont always be this. depends what set we are inserting. I think
  int minimum = -1;
  for (int i = startIndex; i <= endIndex; i++)
  {
      if (minimum != -1)
        minimum = min(minimum, getCountForSubset(itemset, startIndex, endIndex, i));
      else
        minimum = getCountForSubset(itemset, startIndex, endIndex, i);
  }
  
  return minimum;
  
}

/**
 * cMin: finds cMin. used to help find estimation error of an itemset
 * itemset: a full itemset
 * startIndex: first position of the itemset to consider
 * endIndex: last position of the itemset to consider
 */

int Trie::cMin(std::vector<int> & itemset, int startIndex, int endIndex, double Dk)
{
  int maximum = 0;
  int integerDk = ceil(Dk);

	for (int i = startIndex; i <= endIndex - 1; i++) {
		for (int j = i+1; j <= endIndex; j++) {
			if(i != j) {
        //std::cout << "i: " << i << " j: " << j << std::endl;
				//if size = 2, i.e. if Ai intersect Aj = NULL
				if(itemset.size() == 2) {

					//FIXME double stuff with Dk
					maximum = max(maximum, getCountForSubset(itemset, startIndex, endIndex, i) 
            + getCountForSubset(itemset, startIndex, endIndex, j) - abs(integerDk));

				}
				else {

					maximum = max(maximum, getCountForSubset(itemset, startIndex, endIndex, i) 
          + getCountForSubset(itemset, startIndex, endIndex, j) - getCountForSubset(itemset, startIndex, endIndex, i, j));

				}
			}
		}
	}

	return maximum;

}


/**
 * returns the count for a subset of vector itemset that excludes either 1 or two indicies.
 * 
 */
int Trie::getCountForSubset(std::vector<int>& itemset, int startIndex, int endIndex, int excludedIndex1, int excludedIndex2)
{
    //FIXME: add in some error checking for the indices
    if (endIndex >= itemset.size() || startIndex > endIndex) return -1;
    
    Node* current = root;

    //std::cout << "getCountForSubset index1:" << excludedIndex1 << " index2:" << excludedIndex2 << std::endl;

    for ( int i = startIndex; i <= endIndex; i++)
    {
    	if(i != excludedIndex1 && i != excludedIndex2)
    	{
    		Node* tmp = current->findChild(itemset[i]);
            if ( tmp == NULL )
                return 0;
            current = tmp;
    	}
        
    }

    return current->getCount();
}

double Trie::cUpper(std::vector<int> & itemset, double minsup, double dk, double d)
{
  
  //return maxCntBeforeSubsets + cntForSubsets
  double maxCntBeforeSubsets = (1 - pow(d,(itemset.size() -1 )) ) / (1-d);
  double cntForSubsets = minsup * dk - (itemset.size() - 1) *  pow(d,(itemset.size() -1 ));
  return maxCntBeforeSubsets + cntForSubsets;
  //return 1;
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
  //cout << current->content() << " : count :" << current->getCount() << ", err: " << current->getErr() << endl;
}

vector<vector<int> > Trie::getMostFrequentItemset()
{
  vector<int> counts;
  vector<vector<int> > freqItems;
  vector<int> currentSet;
  getMostFrequentItemset(root, freqItems, counts, currentSet, 0);
  //cout << freqItems[0][0] << endl;
  freqItems.push_back(counts);
  return freqItems;
}

void Trie::getMostFrequentItemset(Node * current, vector<vector<int> > &freqItems,
  vector<int> &counts, vector<int> &currentSet, int depth)
{
  if(current == root)
  {
    vector<Node*> c = current->children();
    for (int i = 0; i < c.size(); i++) {
      getMostFrequentItemset(c[i], freqItems, counts, currentSet, depth + 1);
    }
  }
  else
  {
    currentSet.push_back(current->content());
    freqItems.push_back(currentSet);
    counts.push_back(current->getCount());
    vector<Node*> c = current->children();
    for (int i = 0; i < c.size(); i++) {
      getMostFrequentItemset(c[i], freqItems, counts, currentSet, depth + 1);
    }
  }
  
  currentSet.pop_back();
  
  
}

