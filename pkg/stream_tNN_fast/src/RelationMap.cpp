// RelationMap.cpp: Rcpp R/C++ interface for relations 

#include <Rcpp.h>

RcppExport SEXP CreateRelations() {
    
    try {
        std::map<std::pair<int,int>,double>* relations = new std::map<std::pair<int,int>,double>;
        Rcpp::XPtr< std::map<std::pair<int,int>,double> > p(relations, true);
        return( p );
        
    } catch( std::exception &ex ) {		// or use END_RCPP macro
        forward_exception_to_r( ex );
    } catch(...) {
	    ::Rf_error( "c++ exception (unknown reason)" );
    }
    return R_NilValue; // -Wall
}

RcppExport SEXP AddRelations(SEXP x,SEXP i) {
    
    try {
        Rcpp::XPtr< std::map<std::pair<int,int>,double> > relations(x) ;
		    Rcpp::NumericVector inside(i);
    
		for(Rcpp::NumericVector::iterator ii = inside.begin(); ii != inside.end(); ++ii) {
		    for(Rcpp::NumericVector::iterator jj = ii+1; jj != inside.end(); ++jj) {
		        (*relations)[std::make_pair((*ii),(*jj))] += 1;
		    }
		} return R_NilValue;
        
    } catch( std::exception &ex ) {		// or use END_RCPP macro
        forward_exception_to_r( ex );
    } catch(...) {
	    ::Rf_error( "c++ exception (unknown reason)" );
    }
    return R_NilValue; // -Wall
}

RcppExport SEXP DeserializeRelations(SEXP o,SEXP x) {
    try {
        Rcpp::XPtr< std::map<std::pair<int,int>,double> > oldrelations(o) ;
        if(oldrelations) return o;
        Rcpp::DataFrame df(x) ;
        
        std::map<std::pair<int,int>,double>* relations = new std::map<std::pair<int,int>,double>;
        std::vector<int> key1 =  Rcpp::as< std::vector<int> >(df["node1"]);
        std::vector<int> key2 =  Rcpp::as< std::vector<int> >(df["node2"]);
        std::vector<double> weight =  Rcpp::as< std::vector<double> >(df["weight"]);
        
        Rcpp::XPtr< std::map<std::pair<int,int>,double> > p(relations, true);
        
        for(int i=0;i<weight.size();i++) {
          (*relations)[std::make_pair(key1[i],key2[i])] = weight[i];
        }
        
        return( p );
        
        
        return R_NilValue;
        
    } catch( std::exception &ex ) {  	// or use END_RCPP macro
        forward_exception_to_r( ex );
    } catch(...) {
	    ::Rf_error( "c++ exception (unknown reason)" );
    }
    return R_NilValue; // -Wall
}

RcppExport SEXP GetRelations(SEXP x) {
    
    try {
        Rcpp::XPtr< std::map<std::pair<int,int>,double> > relations(x) ;
        Rcpp::NumericVector key1;
        Rcpp::NumericVector key2;
        Rcpp::NumericVector weight;
        
        for( std::map<std::pair<int,int>,double>::iterator ii=relations->begin(); ii!=relations->end(); ++ii) {
            std::stringstream temp;
            key1.push_back((*ii).first.first);
            key2.push_back((*ii).first.second);
            weight.push_back((*ii).second);
        }
        return Rcpp::DataFrame::create(Rcpp::Named("node1")=key1, Rcpp::Named("node2")=key2, Rcpp::Named("weight")=weight);
        
    } catch( std::exception &ex ) {		// or use END_RCPP macro
        forward_exception_to_r( ex );
    } catch(...) {
	    ::Rf_error( "c++ exception (unknown reason)" );
    }
    return R_NilValue; // -Wall
}

RcppExport SEXP AgeRelations(SEXP x,SEXP a) {
    
    try {
        Rcpp::XPtr< std::map<std::pair<int,int>,double> > relations(x) ;
        Rcpp::NumericVector alpha(a);
        for( std::map<std::pair<int,int>,double>::iterator ii=relations->begin(); ii!=relations->end(); ++ii) {
          //#pragma omp task
          {
            (*ii).second = (*ii).second*.5;
            if((*ii).second < .5*alpha[0]) relations->erase(ii);
          }
        }
        return R_NilValue;
        
    } catch( std::exception &ex ) {		// or use END_RCPP macro
        forward_exception_to_r( ex );
    } catch(...) {
	    ::Rf_error( "c++ exception (unknown reason)" );
    }
    return R_NilValue; // -Wall
}

RcppExport SEXP DeleteNodes(SEXP x,SEXP d) {
    
    try {
        Rcpp::XPtr< std::map<std::pair<int,int>,double> > relations(x) ;
        Rcpp::NumericVector del(d);
        for( std::map<std::pair<int,int>,double>::iterator ii=relations->begin(); ii!=relations->end(); ++ii) {
            for(Rcpp::NumericVector::iterator jj = del.begin(); jj != del.end(); ++jj) {
                if((*ii).first.first == (*jj) || (*ii).first.second == (*jj) ) relations->erase(ii);
            }
        }
        return R_NilValue;
        
    } catch( std::exception &ex ) {		// or use END_RCPP macro
        forward_exception_to_r( ex );
    } catch(...) {
	    ::Rf_error( "c++ exception (unknown reason)" );
    }
    return R_NilValue; // -Wall
}

