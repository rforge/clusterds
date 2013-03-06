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
#include <flann/flann.hpp>

RcppExport SEXP CreateCenters(SEXP d) {
    
    try {
        Rcpp::NumericVector rdata(d);
        int colNum = rdata.size();
        
        flann::Matrix<float> input(new float[colNum], 1,  colNum);
        for (int i = 0; i  < colNum; i++) {
            input[0][i] = rdata(i);
        }
        flann::Index<flann::L2<float> >* index = new flann::Index<flann::L2<float> >(input,flann::KDTreeIndexParams(1));
        
        index->buildIndex();
        
        Rcpp::XPtr< flann::Index<flann::L2<float> > > p(index, true);
        
        return p; // -Wall
        
    } catch( std::exception &ex ) {		// or use END_RCPP macro
        forward_exception_to_r( ex );
    } catch(...) {
	    ::Rf_error( "c++ exception (unknown reason)" );
    }
    return R_NilValue; // -Wall
}

RcppExport SEXP AddPoint(SEXP x,SEXP d) {
    
    try {
        Rcpp::XPtr< flann::Index<flann::L2<float> > > index(x);
		Rcpp::NumericVector rdata(d);
        int colNum = rdata.size();
        
        flann::Matrix<float> input(new float[colNum], 1,  colNum);
        for (int i = 0; i  < colNum; i++) {
            input[0][i] = rdata(i);
        }
        
        index->addPoints(input);
        
        return R_NilValue; // -Wall
        
        
    } catch( std::exception &ex ) {		// or use END_RCPP macro
        forward_exception_to_r( ex );
    } catch(...) {
	    ::Rf_error( "c++ exception (unknown reason)" );
    }
    return R_NilValue; // -Wall
}

RcppExport SEXP RemovePoints(SEXP x,SEXP d) {
    
    try {
        Rcpp::XPtr< flann::Index<flann::L2<float> > > index(x);
		Rcpp::NumericVector rdata(d);
        int colNum = rdata.size();
        
        for(Rcpp::NumericVector::iterator ii = rdata.begin(); ii != rdata.end(); ++ii) {
            index->removePoint((*ii));
        }
        
        return R_NilValue; // -Wall
        
        
    } catch( std::exception &ex ) {		// or use END_RCPP macro
        forward_exception_to_r( ex );
    } catch(...) {
	    ::Rf_error( "c++ exception (unknown reason)" );
    }
    return R_NilValue; // -Wall
}




RcppExport SEXP GetPoint(SEXP x,SEXP p,SEXP c) {
    
    try {
        Rcpp::XPtr< flann::Index<flann::L2<float> >  > index(x);
		Rcpp::NumericVector point(p);
		Rcpp::NumericVector colNum(c);
        float* indexPoint = index->getPoint(point[0]);
        Rcpp::NumericVector results;
        
        for(int i=0;i<colNum[0];i++) {
            results.push_back(*(indexPoint+i));
        }
        
        return results; // -Wall
        
    } catch( std::exception &ex ) {		// or use END_RCPP macro
        forward_exception_to_r( ex );
    } catch(...) {
	    ::Rf_error( "c++ exception (unknown reason)" );
    }
    return R_NilValue; // -Wall
}



RcppExport SEXP GetAllPoints(SEXP x,SEXP n,SEXP c) {
    
    try {
        Rcpp::XPtr< flann::Index<flann::L2<float> >  > index(x);
		Rcpp::NumericVector npoints(n);
		Rcpp::NumericVector cn(c);
        int colNum = cn[0];
        float data[colNum];
        for(int i=0;i<colNum;i++) {
            data[i] = 0;
            i++;
        }
        flann::Matrix<float> dataset = flann::Matrix<float>(data,1,colNum);
        
        std::vector< std::vector<int> > indices;
        std::vector< std::vector<float> > dists;
        
        index->knnSearch(dataset,indices,dists,npoints[0],flann::SearchParams(-1));
        
        std::sort (indices[0].begin(), indices[0].end()); 
        
        Rcpp::NumericMatrix results(indices[0].size(), colNum);
        Rcpp::IntegerVector rownames;
        int i = 0;
        for(std::vector<int>::iterator ii = indices[0].begin(); ii != indices[0].end(); ++ii) {
            float* indexPoint = index->getPoint(*(ii));
            rownames.push_back(*(ii));
            for(int j=0;j<colNum;j++) {
                results(i,j)=(*(indexPoint+j));
            }
            i++;
        }
        
        Rcpp::List dimnms = Rcpp::List::create(rownames, Rcpp::Range(1,colNum));
        results.attr("dimnames") = dimnms;
        
        return results;
        
    } catch( std::exception &ex ) {		// or use END_RCPP macro
        forward_exception_to_r( ex );
    } catch(...) {
	    ::Rf_error( "c++ exception (unknown reason)" );
    }
    return R_NilValue; // -Wall
}



RcppExport SEXP RadiusSearch(SEXP x,SEXP p,SEXP d) {
    
    try {
        Rcpp::XPtr< flann::Index<flann::L2<float> >  > index(x);
		Rcpp::NumericVector rdata(p);
		Rcpp::NumericVector radius(d);
        int colNum = rdata.size();
        float data[colNum];
        int i=0;
        for(Rcpp::NumericVector::iterator ii = rdata.begin(); ii != rdata.end(); ++ii) {
            data[i] = (*ii);
            i++;
        }
        flann::Matrix<float> dataset = flann::Matrix<float>(data,1,colNum);
        
        std::vector< std::vector<int> > indices;
        std::vector< std::vector<float> > dists;
        
        index->radiusSearch(dataset,indices,dists,radius[0],flann::SearchParams(-1));
        
        Rcpp::IntegerVector iresults( indices[0].begin(), indices[0].end() );
        Rcpp::IntegerVector dresults( dists[0].begin(), dists[0].end() );
        
        
        
        
        return Rcpp::DataFrame::create(Rcpp::Named("indices")=iresults, Rcpp::Named("dist")=dresults);
        
        
    } catch( std::exception &ex ) {		// or use END_RCPP macro
        forward_exception_to_r( ex );
    } catch(...) {
	    ::Rf_error( "c++ exception (unknown reason)" );
    }
    return R_NilValue; // -Wall
}