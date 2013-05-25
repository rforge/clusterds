// FLANN.cpp: Rcpp R/C++ interface for kd-tree

#include <Rcpp.h>
#include <flann/flann.hpp>
#include <fstream>

RcppExport SEXP DeserializeFlann(SEXP x,SEXP m) {
    try {
      
        Rcpp::XPtr< flann::Index<flann::L2<float> >  > oldindex(x);
        if(oldindex)
          return x;
        
        if(Rf_isNull(m))
          ::Rf_error("Please serialize");  
        
        Rcpp::NumericMatrix dataset(m);
        
        flann::Matrix<float> input(new float[dataset.nrow()*dataset.ncol()], dataset.nrow(),  dataset.ncol());
        
        //#pragma omp parallel for
        for( int j = 0; j < dataset.nrow(); j++) {
          for (int i = 0; i  < dataset.ncol(); i++) 
            input[j][i] = dataset(j,i);
        }
        
        flann::Index<flann::L2<float> >* index = new flann::Index<flann::L2<float> >(input,flann::KDTreeSingleIndexParams());
        
        index->buildIndex();
        
        Rcpp::XPtr< flann::Index<flann::L2<float> > > p(index, true);
        
        return p; // -Wall
        
    } catch( std::exception &ex ) {    // or use END_RCPP macro
        forward_exception_to_r( ex );
    } catch(...) {
	    ::Rf_error( "c++ exception (unknown reason)" );
    }
    return R_NilValue; // -Wall
}

RcppExport SEXP CreateCenters(SEXP d) {
    
    try {
        Rcpp::NumericVector rdata(d);
        int colNum = rdata.size();
        
        flann::Matrix<float> input(new float[colNum], 1,  colNum);
        
        for (int i = 0; i  < colNum; i++) {
            input[0][i] = rdata(i);
        }
        flann::Index<flann::L2<float> >* index = new flann::Index<flann::L2<float> >(input,flann::KDTreeSingleIndexParams());
        
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
        //int colNum = rdata.size();
        
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
        
	float *data = new float[colNum];
        for(int i=0;i<colNum;i++) {
            data[i] = 0;
            i++;
        }
        
	flann::Matrix<float> dataset = flann::Matrix<float>(data,1,colNum);
        
	delete [] data;

        std::vector< std::vector<int> > indices;
        std::vector< std::vector<float> > dists;
        
        index->knnSearch(dataset,indices,dists,npoints[0],flann::SearchParams(-1));
        
        std::sort (indices[0].begin(), indices[0].end()); 
        
        Rcpp::NumericMatrix results(indices[0].size(), colNum);
        Rcpp::IntegerVector rownames;
        
        int num = indices[0].size();
        
        //#pragma omp parallel for ordered schedule(dynamic)
        for(int i=0;i<num;i++) {
            float* indexPoint = index->getPoint(indices[0][i]);
            for(int j=0;j<colNum;j++) {
                results(i,j)=(*(indexPoint+j));
            }
            
            //#pragma omp ordered
            rownames.push_back(indices[0][i]);
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



RcppExport SEXP RadiusSearch(SEXP x,SEXP p,SEXP d,SEXP w) {
    
    try {
        Rcpp::XPtr< flann::Index<flann::L2<float> >  > index(x);
    		Rcpp::NumericVector rdata(p);
      	Rcpp::NumericVector radius(d);
      	Rcpp::NumericVector weights(w);
        int colNum = rdata.size();
	    
	float *data = new float[colNum];
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
        
        int num = indices[0].size();
        
        if(num>0) {
          float partialWeight = 1/num;
          
          //float tempCenters[num][colNum];
          float *tempCenters = new float[num * colNum];
          
          for(int i=0;i<num;i++) {
            float* indexPoint = index->getPoint(indices[0][i]);
            std::string s;
            std::stringstream out;
            out << indices[0][i];
            s = out.str();
            float weight = weights[s];
            for(int j=0;j<colNum;j++) {
                //tempCenters[i][j] = (*(indexPoint+j)*weight+data[j]*partialWeight)/(weight+partialWeight);
                tempCenters[i*num+j] = (*(indexPoint+j)*weight+data[j]*partialWeight)/(weight+partialWeight);
            }
          }
          
          for(int i=0;i<num;i++) {
            bool valid=true;
            for(int j=0;j<num;j++) {
              if(i!=j) {
                float sum=0;
                for(int k=0;k<num;k++) {
                  //float temp=(tempCenters[i][k]-tempCenters[j][k]);
                  float temp=(tempCenters[i*num+k]-tempCenters[j*num+k]);
                  sum += temp*temp;
                }
                if(sum<(radius[0]*radius[0])) {
                  valid=false;
                }
              }
            }
            if(valid){
              float* indexPoint = index->getPoint(indices[0][i]);
              for(int j=0;j<colNum;j++) {
                  //*(indexPoint+j) = tempCenters[i][j];
                  *(indexPoint+j) = tempCenters[i*num+j];
              }
            }
          }
	  delete [] tempCenters;
        }
        
	delete [] data;
        
        return Rcpp::DataFrame::create(Rcpp::Named("indices")=iresults, Rcpp::Named("dist")=dresults);
        
        
    } catch( std::exception &ex ) {		// or use END_RCPP macro
        forward_exception_to_r( ex );
    } catch(...) {
	    ::Rf_error( "c++ exception (unknown reason)" );
    }
    return R_NilValue; // -Wall
}
