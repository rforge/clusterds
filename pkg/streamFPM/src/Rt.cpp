#include <iostream>
#include <vector>
#include <math.h>
#include "Rcpp.h"
#include "Trie.h"

class RTrie: private Trie {
public:
    RTrie();
    ~RTrie();
    void addSet(Rcpp::IntegerVector itemset);
    bool search(Rcpp::IntegerVector itemset);
    bool deleteSet(Rcpp::IntegerVector itemset);
    bool addAllSets(Rcpp::IntegerVector itemset);
    bool updateAllSets(Rcpp::IntegerVector itemset, int transNum, double decayRate, double minsup, double dk);
    bool updateSet(Rcpp::IntegerVector itemset, int tid);
    int  size(Rcpp::IntegerVector itemset);
    void printAll();
 };
 
 RTrie::RTrie()
{

}

RTrie::~RTrie()
{
  
}

void RTrie::addSet(Rcpp::IntegerVector itemset)
{
  //Rcpp::IntegerVector xx(itemset);
  this->addWord(Rcpp::as<std::vector<int> >(itemset));
}

bool RTrie::search(Rcpp::IntegerVector itemset)
{
 // Rcpp::IntegerVector xx(itemset);
	return( this->searchWord(Rcpp::as<std::vector<int> >(itemset))); 
}

bool RTrie::deleteSet(Rcpp::IntegerVector itemset)
{
  
	return( this->deleteWord(Rcpp::as<std::vector<int> >(itemset))); 
}

bool RTrie::addAllSets(Rcpp::IntegerVector itemset)
{
  return false;
}

bool RTrie::updateSet(Rcpp::IntegerVector itemset, int tid)
{
  
  return false;
}

bool RTrie::updateAllSets(Rcpp::IntegerVector itemset, int transNum, double decayRate, double minsup, double dk)
{
  
  std::vector<int> e = Rcpp::as<std::vector<int> >(itemset);
  std::sort (e.begin(), e.end());

  for (int first = 0; first < e.size(); first++) {
    
    updateWord(e, transNum, decayRate, minsup,  dk, 0, first, NULL);
              
  }

  
}

int  RTrie::size(Rcpp::IntegerVector itemset)
{
  std::vector<int> e = Rcpp::as<std::vector<int> >(itemset);
  return e.size();
}

void RTrie::printAll()
{
 this->printTree(NULL);
}


RCPP_MODULE(rtrie)
{
  using namespace Rcpp;
  class_<RTrie>("RTrie")
  .default_constructor()
  .method("addSet", &RTrie::addSet)
  .method("updateSet", &RTrie::updateSet)
  .method("addAllSets", &RTrie::addAllSets)
  .method("updateAllSets", &RTrie::updateAllSets)
  .method("search", &RTrie::search)
  .method("deleteSet", &RTrie::deleteSet)
  .method("size", &RTrie::size)
  .method("printAll", &RTrie::printAll)
  ;
}