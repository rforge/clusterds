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
    minsup_ = 0.001;
    insertSupport_ = 0.0008;
    double pruningSupport_ = 0.0007;
    double decayRate_ = 0.999;
}

Trie::~Trie()
{
    // Free memory
}

void Trie::addItemsetDirectly(std::vector<int> itemset)
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
    }
}


bool Trie::searchItemset(std::vector<int> itemset)
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



//FIXME ALL OF THIS
bool Trie::deleteItemset(std::vector<int> itemset)
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
      current->removeChild(itemset[i]);
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
      current->removeChild(itemset[i]);
      if(current->children().size() == 0 && current->wordMarker() == false)
        return 1;
    }
    else if(temp == 2) {
      return 2;
    }
    
  }
  
  
}

//FIXME rename as deleteNodeAndDescendents
bool Trie::deleteNodeAndChildren(Node* current)
{
  vector<Node*> children = current->children();
  for(int i = 0; i < children.size(); i++)
  {
    deleteNodeAndChildren(children.at(i));
  }
  
  delete current;
  
  //fixme, need to tell about node that i am deleted
  return true;
}

void Trie::deleteDescendentNodes(Node* current)
{
  vector<Node*> children = current->children();
  
  if(children.size() == 0)
    return;
    
  for(int i = children.size()-1; i >= 0; i--)
  {
    Node* tmp = children.at(i);
    deleteDescendentNodes(children.at(i));
    current->removeChild(tmp->content());
  }
  
}

bool Trie::deleteChildNodeAndDescendents(Node* parent, int content)
{
  Node* child = parent->findChild(content);
  deleteDescendentNodes(child);
  parent->removeChild(content);
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
bool Trie::updateWordRecursion(std::vector<int> & itemset, int k, double decay, double minsup, double dk, int len, int first, Node* current)
{
    if (current != root)
    {
      //updates the count and estimate of itemsets. 1-itemsets get error = 0. 
      if(len == 1)
      {
        //std::cout << "updating 1 itemset: " << current->content() << std::endl;
        //current->setCount( current->getCount() + 1);
        current->setCount( (current->getCount() * pow(decay, k - current->getId())) + 1);
        current->setErr(0);
        current->setId(k);
      }
      else
      {
        //std::cout << "here2 update set: " << current->content() << std::endl;
        current->setCount( (current->getCount() * pow(decay, k - current->getId()) ) + 1);
        current->setErr(current->getErr() * pow(decay, k - current->getId()));
        current->setId(k);
      }

    }
    
    //recursion for traversing tree
    for ( int i = first; i < itemset.size(); i++ )
    {
        //std::cout << "set: " << i << " starting with item:" << itemset[i] <<endl;
        Node* tmp = current->findChild(itemset[i]);
        //FIXME: FIX LEN
        
        //if tmp == null, have reached left node. return
        if ( tmp == NULL )
        {
          return true;
        }
        else {
            //std::cout << "calling recursion" << std::endl;
            updateWordRecursion(itemset, k, decay, minsup,  dk, len + 1, i+1, tmp);
        }

        //FIXME minsup should be pruning support
        //FIXME  bad stuff with doubles
        //std::cout << tmp->getCount()/dk << " :: " << minsup << std::endl;
        if(tmp->getCount()/dk < pruningSupport_  && len+1 > 1) {
            //std::cout << "remove node" << std::endl;
            deleteChildNodeAndDescendents(current, itemset[i]);
        }
        
    }

    return true;
}


//calls insertionPhase with current = root, since RT doesn't have access to root
bool Trie::insertionPhase(std::vector<int> & itemset, int k, double d, double minsup, double dk, int len, int first)
{
    lastTID_ = k;
    totalSetsWithDecay_ = dk;
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
    
      
      if(cmax_double/dk  >= insertSupport_)
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
void Trie::printTree(Node * current, int max_depth, int depth)
{
  if(depth <= max_depth  || max_depth < 0) {
      if(current == NULL)
        current = root;
      else{
        std::cout << current->content() << " : count :" << current->getCount() << ", err: " << current->getErr() << std::endl;
      }
      vector<Node*> c = current->children();
      for (int i = 0; i < c.size(); i++) {
        printTree(c[i], max_depth, depth + 1);
      }
  }

}

vector<vector<int> > Trie::getMostFrequentItemset(double dk)
{
  vector<int> counts;
  vector<vector<int> > freqItems;
  vector<int> currentSet;
  getMostFrequentItemset(root, freqItems, counts, currentSet, 0, dk);
  //cout << freqItems[0][0] << endl;
  freqItems.push_back(counts);
  return freqItems;
}

void Trie::getMostFrequentItemset(Node * current, vector<vector<int> > &freqItems,
  vector<int> &counts, vector<int> &currentSet, int depth, double dk)
{
  if(current == root)
  {
    //std::cout << "root " << std::endl;
    vector<Node*> c = current->children();
    for (int i = 0; i < c.size(); i++) {
      getMostFrequentItemset(c[i], freqItems, counts, currentSet, depth + 1, dk);
    }
  }
  else
  {
    //std::cout << "current node: " << current->content() << " depth: " << depth << " sup: " << current->getCount()/dk << std::endl;
    if(current->getId() < lastTID_) {
      current->setCount( (current->getCount() * pow(decayRate_, lastTID_ - current->getId()) ));
      current->setErr(current->getErr() * pow(decayRate_, lastTID_ - current->getId()));
      current->setId(lastTID_);
      //std::cout << "updated values count: " << current->getCount()<< std::endl;
    }
    //if the current set meets the minsup, add it to frequent itemsets and check children
    
    if(current->getCount()/dk >= minsup_) {
      //std::cout << "adding " << current->content() << " to itemsets" << std::endl;
      //for (int i = 0; i < currentSet.size(); i++) {
      //  cout << currentSet[i] << ", ";
      //}
      //cout << endl;
      currentSet.push_back(current->content());
      //std::cout << "here1" << std::endl;
      freqItems.push_back(currentSet);
      //std::cout << "here2" << std::endl;
      counts.push_back(current->getCount());
      //std::cout << "here3" << std::endl;
      vector<Node*> c = current->children();
      //std::cout << "calling for " << c.size() << "children" << std::endl;
      for (int i = 0; i < c.size(); i++) {
        //std::cout << "here 5" << std::endl;
        getMostFrequentItemset(c[i], freqItems, counts, currentSet, depth + 1, dk);
      }
      currentSet.pop_back();
    }
    
  }
  
  
  
  
}

bool Trie::updateParams(double decayRate, double minsup, double insertSupport, double pruningSupport) 
{
  if(decayRate > 0)
    decayRate_ = decayRate;
  if(minsup > 0)
    minsup_ = minsup;
  if(insertSupport > 0)
    insertSupport_ = insertSupport;
  if(pruningSupport > 0)
    pruningSupport_ = pruningSupport;
}

void Trie::printSupports()
{
  std::cout << "minsup: " << minsup_  << std::endl;
  std::cout << "insSup: " << insertSupport_ << std::endl;
  std::cout << "pruningSup: " << pruningSupport_ << std::endl;
  std::cout << "decayRate: " << decayRate_ << std::endl;
}