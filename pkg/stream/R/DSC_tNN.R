#######################################################################
# stream -  Infrastructure for Data Stream Mining
# Copyright (C) 2013 Michael Hahsler, Matthew Bolanos, John Forrest 
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


tNN <- setRefClass("tNN",
  fields = list(
    ### parameters (micro-clustering)
    r			= "numeric",
    measure			= "character",
    lambda			= "numeric",
    gap_time		= "integer",
    ### noise: min. weight for micro-clusters given as a 
    ### percentile of the total weight of the clustering (i.e.,
    ### noise% of the data points is considered noise)
    noise			= "numeric", 
    
    ### used internally
    distFun			= "ANY",
    decay_factor		= "numeric",
    debug			= "logical",
    
    ### data
    weights			= "numeric",
    total_weight		= "numeric",
    npoints			= "integer",
    centers			= "data.frame",
    last_update  = "integer",
    relations 		= "hash",
    
    ### Macro-clustering
    shared_density		= "logical",
    ### alpha: intersection factor (area of the intersection)
    alpha			= "numeric",
    ### k: number of macro-clusters (alternative to alpha)
    k			= "integer",
    ### minweights: min. weight for macro-clusters 	
    minweight		= "numeric"
  ),
  
  
  methods = list(
    initialize = function(
      r		= 0.1,
      lambda		= 1e-3,
      gap_time  = 1000L,
      noise		= 0.01,
      measure		= "Euclidean",
      shared_density		= FALSE,
      alpha 		= 0,
      k		= 0,
      minweight	= 0.1
    ) {
      
      relations 		<<- hash()
      r			<<- r
      lambda		<<- lambda
      gap_time	<<- gap_time
      decay_factor	<<- 2^(-lambda)
      noise		<<- noise
      measure		<<- measure
      shared_density		<<- shared_density
      alpha		<<- alpha
      minweight		<<- minweight
      
      if(is.null(k))
        k		<<- 0L
      else
        k		<<- as.integer(k)
      
      weights		<<- numeric()
      total_weight	<<- 0
      npoints		<<- 0L
      centers		<<- data.frame()
      
      distFun		<<- pr_DB[[measure]]
      
      .self
    }
    
  ),
)


DSC_tNN <- function(r = 0.1, lambda = 1e-3,  gap_time=1000L, noise = 0.01, 
  measure = "Euclidean", 
  shared_density = FALSE, alpha = 0, k=0, minweight = 0) {
  
  if(k==0 && alpha==0 && shared_density) {
    warning("You have to specify at least k or alpha! Using default alpha=.25 and minweight=0.1.")
    minweight <- 0.1
    alpha <- 0.25
  }
  
  tNN <- tNN$new(r, lambda, as.integer(gap_time), 
    noise, measure, shared_density, alpha, k, minweight)
  l <- list(description = "tNN", RObj = tNN)
  class(l) <- c("DSC_tNN", "DSC_Micro", "DSC_R", "DSC")
  l
}

tNN$methods(list(
  
  cluster = function(newdata, debug = FALSE) {
    'Cluster new data.' ### online help
    
    newdata <- as.data.frame(newdata)
    
    if(debug) cat("Debug clustering for tNN!\n")
    
    for(i in 1:nrow(newdata)) {
      npoints <<- npoints + 1L
      
      if(debug && !i%%100) cat("Processed",i,"points\n")
      
      ### decay and remove clusters
      if(decay_factor<1 && !npoints%%gap_time) {
        ### remove clusters
        weight_remove <- .5
        remove <- which(get_current_weights() <= weight_remove)
        
        if(debug) cat("  - Removing", length(remove), "micro-clusters.\n")
        
        if(length(remove)) {
          # remove relations
          if(shared_density) {  
            keys <- keys(relations)
            mcs <- rownames(centers)
            
            #remove microclusters in relations
            removekeys_id <- c(
              grep(paste("^",mcs[remove],"-",
                sep="",collapse="|"), 
                keys, value = FALSE), 
              grep(paste("-",mcs[remove],"$"
                ,sep="",collapse="|"), 
                keys, value = FALSE)
            )
            removekeys_id <- unique(removekeys_id)
            removekeys <- keys[removekeys_id]
            
            if(length(removekeys)>0) {
              if(debug) cat("  - Removing relation (cluster)",
                paste(removekeys, collapse=", "), 
                "\n")
              
              del(removekeys, relations)
            }
          }
          
          # remove weak relations
          if(length(relations) > 0) {
            keys <- keys(relations)
            vals <- values(relations, simplify=FALSE)
            
            ws <- sapply(vals, FUN=function(v) v[2]*decay_factor^(npoints-v[1]))
            
            removekeys <- keys[ws <= weight_remove]
            
            if(length(removekeys)>0) {
              if(debug) cat("  - Removing relation (relation)",
                paste(removekeys, collapse=", "), 
                "\n")
              
              del(removekeys, relations)
            }
          }
          
          # remove microclusters
          weights <<- weights[-remove]
          last_update <<- last_update[-remove]
          centers <<- centers[-remove,]
        }
      }
      
      ### process new point
      point <- newdata[i,]
      mcs <- rownames(centers) ### names
      
      total_weight <<- total_weight * decay_factor + 1
      
      if(nrow(centers)<1) {   ### create first micro-cluster
        weights <<- 1
        centers <<- as.data.frame(point)
        rownames(centers) <<- 1
        last_update <<- npoints
        
        if(debug) cat("  + Creating micro-cluster with ID 1.\n")
        
      } else {
        inside <- which(dist(point, centers, method=distFun) < r)
        
        if(length(inside) < 1) { ### new cluster (cannot have shared density)
          weights <<- c(weights, 1)
          centers <<- rbind(centers,point)
          rownames(centers)[nrow(centers)] <<-
            as.integer(rownames(centers)[nrow(centers)-1]) + 1L
          last_update <<- c(last_update, npoints)
          
          if(debug) cat("  + Creating micro-cluster with ID",
            rownames(centers)[nrow(centers)], "\n")
          
        }else{ ### update existing cluster
          partialweight <- 1/length(inside) 
          
          newCenters <- data.frame(matrix((as.numeric(as.matrix(
            centers[inside,]) * rep(weights[inside]) + 
              rep(as.numeric(point) * partialweight,each=length(inside)))) / 
              rep(partialweight+weights[inside], ncol(point)),
            ncol=ncol(point)),
            row.names=rownames(centers[inside,]))
          
          distance <- dist(newCenters,method=distFun)
          
          test <- apply(distance, 1, function(x) all(x>r|x==0) )
          if(length(which(test)) > 0) 
            centers[inside[which(test)],] <<- newCenters[which(test),]
          
          weights[inside] <<- weights[inside] * 
            decay_factor^(npoints-last_update[inside]) + partialweight
          last_update[inside] <<- npoints
          
          # shared density
          if(shared_density && length(inside) > 1) {
            relationUpdate <- outer(mcs[inside], mcs[inside], 
              function(x,y) paste(x,y,sep="-") )
            relationUpdate <- relationUpdate[upper.tri(relationUpdate)]
            
            if(length(relationUpdate) > 0){
              if(debug) cat("  + Updating/Create Relations",
                paste(relationUpdate, collapse=", "), "\n")
              
              for(j in relationUpdate) {
                ### relation is c(last_update, count)
                rel <- relations[[j]]
                if(is.null(rel)) rel <- c(npoints, 1)
                else rel <- c(npoints, 
                  rel[2] * decay_factor^(npoints-rel[1]) + 1)
                relations[[j]] <<- rel
              }
            }
          }
        }
      }
    }
  },
  
  get_current_weights = function() {
    weights * decay_factor^(npoints-last_update) 
  },
  
  # find strong MCs
  strong_mcs = function(weak=FALSE) {
    
    ws <- get_current_weights()
    o <- order(ws, decreasing=FALSE)
    
    # first element represents weight of already deleted MCs!
    cs <- cumsum(c(total_weight-sum(ws), ws[o]))
    
    if(weak)
      o[(cs < total_weight*noise)[-1]]
    else	
      o[(cs >= total_weight*noise)[-1]]
  },
  
  get_shared_density = function(matrix=FALSE) {
    mc_weights <- get_current_weights()
    mcs <- rownames(centers)
    
    rel <- t(sapply(strsplit(keys(relations), "-"), as.integer))
    rel <-  matrix(match(rel, mcs), ncol=2) ### translate from names to index
    
    s <- matrix(0, ncol=length(mcs), nrow=length(mcs))
    
    if(nrow(rel) > 0){
      ### fix decay
      vals <- sapply(values(relations, simplify=FALSE), 
        FUN=function(v) v[2]* decay_factor^(npoints-v[1]) )
      
      ### get shared density relative to the density on the clusters
      avg_weight <- apply(rel, MARGIN=1, FUN= function(x) mean(mc_weights[x]))
      ss <- vals/avg_weight
      
      ### unconnected is 2 times the largest distance
      for(i in 1:nrow(rel)) s[rel[i,2], rel[i,1]] <- s[rel[i,1], rel[i,2]] <- ss[i]
    }
    
    strong <- strong_mcs()
    s <- s[strong, strong]
    if(!matrix) s <- as.simil(s)
    s
  },
  
  get_membership_weights = function() {
    s <- get_shared_density()
    
    nclusters <- nrow(get_microclusters())
    if(nclusters <1) return(list(assignment=integer(0), weight=numeric(0)))
    
    if(nrow(s)<2) assignment <- 1:nclusters
    else if(alpha>0) { ### use alpha
      s[s < alpha] <- 0
      s[s>0] <- 1
      d <- 1-s
      assignment <- cutree(hclust(d, method="single"), h=.5)
    }else{ ### use k
      if(alpha<0) warning("You need to specify at leasy alpha or k!")
      d <- 1/(1+s)
      
      ### FIXME: If k>number of connected components then components would
      ###  be merged randomly! So we add for these the redular distance!
      d2 <- dist(get_microclusters(), method=distFun) 
      unconnected <- d==1
      d[unconnected] <- d[unconnected] + d2[unconnected]
      
      assignment <- cutree(hclust(d, method="single"), k=k)
    }
    
    ### aggregate macro-cluster weights
    w <- get_microweights()
    w <- aggregate(w, by=list(assignment), FUN=sum)$x
    
    ### deal with k and minweight (only if alpha is given!)
    if(alpha>0) {
      if(k>0 && length(w)>k) {
        take <- order(w, decreasing=TRUE)[1:k]
        w <- w[take]
        assignment <- match(assignment, take)
      }
      if(minweight>0) {
        take <- which(w>=(minweight*sum(w)))
        w <- w[take]
        assignment <- match(assignment, take)
      }
    }
    
    return(list(assignment=assignment, weight=w))
  },
  
  get_microclusters = function() {
    ### we have to rename the micro-clusters
    mc <- centers
    mc <- mc[strong_mcs(),]
    rownames(mc) <- NULL
    
    if(nrow(mc)<1) return(data.frame())
    
    mc
  },
  
  get_microweights = function() {
    get_current_weights()[strong_mcs()]
  },
  
  get_macroclusters = function() {
    if(!shared_density) stop("No macro-clusters available (use shared_density)!")
    
    mcs <- get_microclusters()
    if(nrow(mcs)<1) return(data.frame())
    
    mcw <- get_microweights()
    
    mw <-  get_membership_weights()
    assignment <- mw$assignment
    uniqueassign <- na.omit(unique(assignment))
    
    if(length(uniqueassign) <1) return(data.frame())
    
    
    
    ### find weighted centroids
    as.data.frame(t(sapply(uniqueassign, FUN=function(i) {
      take <- which(assignment==i)
      colSums(mcs[take,]*mcw[take])/sum(mcw[take])	
    })))
  },
  
  get_macroweights = function() {
    if(!shared_density) stop("No macro-clusters available (use shared density)!")
    get_membership_weights()$weight
  },
  
  microToMacro = function(micro=NULL) {
    if(is.null(micro)) micro <- 1:nrow(get_microclusters()) ### is nclusters
    mw <- get_membership_weights()
    
    structure(mw$assignment[micro], names=micro)
  }
))

get_microclusters.DSC_tNN <- function(x) x$RObj$get_microclusters()
get_microweights.DSC_tNN <- function(x) x$RObj$get_microweights()

get_macroclusters.DSC_tNN <- function(x) x$RObj$get_macroclusters()
get_macroweights.DSC_tNN <- function(x) x$RObj$get_macroweights()

microToMacro.DSC_tNN <- function(x, micro=NULL) x$RObj$microToMacro(micro=micro)
get_shared_density <- function(x, matrix=FALSE) x$RObj$get_shared_density(matrix=matrix)

### special plotting for DSC_tNN
### FIXME: only show edges that really are used
plot.DSC_tNN <- function(x, dsd = NULL, n = 1000,
  col_points="gray",
  col_clusters="red",
  weights=TRUE,
  scale=c(1,5),
  cex =1,
  pch=NULL,
  ...,
  method="pairs",
  type=c("auto", "micro", "macro")) {
  
  NextMethod()
  
  
  if(x$RObj$shared_density && type %in% c("macro")
    && (ncol(x$RObj$centers)<=2 || method=="plot")) {
    
    p <- get_centers(x, type="micro")
    
    if(nrow(p)>0) {
      points(p, col="black")
      
      ### add threshold circles
      for(i in 1:nrow(p)){
        lines(ellipsePoints(x$RObj$r, x$RObj$r, 
          loc=as.numeric(p[i,]), n=60),
          col = "black", lty=3)
      }
      
      ### add edges connecting macro-clusters
      s <- get_shared_density(x, matrix=TRUE)
      s[lower.tri(s)] <- NA
      
      edges <- which(s>x$RObj$alpha, arr.ind=TRUE)
      
      if(length(edges)>0) { # length instead of nrow (s can be empty!)
        edges <- cbind(edges, 
          w=apply(edges, MARGIN=1, FUN=function(ij) s[ij[1], ij[2]]))
        
        #edges <- cbind(edges, stream:::map(edges[,3], range=c(1,5)))
        edges <- cbind(edges, map(edges[,3], range=c(1,5)))
        
        for(i in 1:nrow(edges)){
          lines(rbind(p[edges[i,1],],p[edges[i,2],]),
            col="black",lwd=edges[i,4])
        }
      }   
    }
  }
}
