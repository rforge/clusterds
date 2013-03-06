// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; tab-width: 8 -*-
//
// DataFrame.cpp: Rcpp R/C++ interface class library data frame example
//
// Copyright (C) 2011        Dirk Eddelbuettel and Romain Francois
//
// This file is part of Rcpp.
//
// Rcpp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// Rcpp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Rcpp.  If not, see <http://www.gnu.org/licenses/>.

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
            (*ii).second = (*ii).second*.5;
            if((*ii).second < .5*alpha[0]) relations->erase(ii);
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

