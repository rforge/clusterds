tNN_fast <- setRefClass("tNN_fast",
    fields = list(
    ### parameters (micro-clustering)
    r			= "numeric",
    measure			= "character",
    lambda			= "numeric",
    decay_interval		= "integer",
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
    rel = "ANY",
    flann = "ANY",
    nclusters = "integer",
    columns = "integer",
		
		### Macro-clustering
		macro			= "logical",	# do macro?
		### alpha: intersection factor (area of the intersection)
		alpha			= "numeric",
		### minweights: min. weight for macro-clusters 	
		minweight		= "numeric",
		### k: number of macro-clusters (alternative to )
		k			= "numeric"
		),


	methods = list(
		initialize = function(
			r		= 0.1,
			k		= 0,
			lambda		= 1e-3,
			decay_interval  = 1000L,
			minweight	= 0.1,
			noise		= 0.01,
			alpha 		= 0.25,
			measure		= "Euclidean",
			macro		= TRUE
			) {

		    r			<<- r
		    lambda		<<- lambda
		    decay_interval	<<- decay_interval
		    decay_factor	<<- 2^(-lambda*decay_interval)
		    minweight		<<- minweight
		    noise		<<- noise
		    alpha		<<- alpha
		    measure		<<- measure
		    macro		<<- macro
        rel <<- .Call("CreateRelations", PACKAGE="stream")
      
        if(is.null(k))
    	    k		<<- 0
		    else
			    k		<<- k

		    weights		<<- numeric()
		    total_weight	<<- 0
		    npoints		<<- 0L
        flann <<- NULL
		    nclusters <<- 0L

		    distFun		<<- pr_DB[[measure]]

		    .self
		  }

		),
	)


  DSC_tNN_fast <- function(r = 0.1, k=0, alpha = 0, minweight = 0, lambda = 1e-3, decay_interval=1000L, noise = 0.01, measure = "Euclidean", macro = TRUE) {
    if(k==0 && alpha==0) {
	    warning("You have to specify at least k or alpha! Using default alpha=.25 and minweight=0.1.")
	    minweight <- 0.1
	    alpha <- 0.25
    }

    tNN_fast <- tNN_fast$new(r, k, lambda, as.integer(decay_interval), minweight, noise, alpha, measure, macro)

    l <- list(description = "tNN_fast", RObj = tNN_fast)
    class(l) <- c("DSC_tNN_fast", "DSC_Micro", "DSC_R", "DSC")

    l
  }

  tNN_fast$methods(cluster = function(newdata, debug = FALSE) {
      'Cluster new data.' ### online help
      newdata <- as.data.frame(newdata)
  
  	  if(debug) cat("Debug cluster for tNN_fast!\n")
      
      for(i in 1:nrow(newdata)) {
  	    npoints <<- npoints + 1L
        if(debug && !i%%100) cat("Processed",i,"points\n")
  
  		  ### decay and remove clusters
  		  if(decay_factor<1 && !npoints%%decay_interval) {
  		    #decrease weight for microclusters
  		    weights <<- weights * decay_factor
  		    total_weight <<- total_weight * decay_factor
          weight_remove <- .5
  		    remove <- which(weights <= weight_remove)
          
          if(length(remove)>0) {
  			    #remove microclusters
  			    removeKeys <- as.integer(names(weights)[remove])
  			    weights <<- weights[-remove]
        
  			    .Call("RemovePoints",flann, removeKeys, PACKAGE="stream")
  			    .Call("DeleteNodes", rel, removeKeys, PACKAGE="stream")
          }
  
  		    ### decay and remove weak relations
  		    if(macro) {  
            .Call("AgeRelations", rel, alpha, PACKAGE="stream")
          }
  		  }
  
    		### process new point
    		point <- newdata[i,]
    		
    		total_weight <<- total_weight +1
        
        if(length(weights)==0) {
  		    #create first micro-cluster
  		    weights <<- 1
          names(weights) <<- "0"
          columns <<- length(as.numeric(point))
          nclusters <<- nclusters + 1L
  		    flann <<- .Call("CreateCenters",as.numeric(point), PACKAGE="stream")
  		  } else {
  		    
  		    inside <- .Call("RadiusSearch",flann,as.numeric(point),r^2,weights, PACKAGE="stream")
          inside <- inside[,1]

        if(length(inside)<1) { ### new cluster
  			  weights <<- c(weights, 1)
          names(weights) <<- c(names(weights[1:length(weights)-1]),nclusters)
  			  .Call("AddPoint",flann,as.numeric(point),columns, PACKAGE="stream")
  			  if(debug) cat("  + Creating Cluster", nclusters, "\n")
  			  nclusters <<- nclusters + 1L;


		    }else{ ### update existing cluster

		  	  partialweight <- 1/length(inside) 
		  	  
          
			    weights[as.character(inside)] <<- weights[as.character(inside)] + partialweight

			    if(macro && length(inside)>1) {
            .Call("AddRelations", rel, inside, PACKAGE="stream")
          }
		    }
		  }	   
    }
  },
                   
                   
                   ###########################################################################
                   ### helpers
                   
                   # find strong MCs
                   strong_mcs = function(weak=FALSE) {
                     o <- order(weights, decreasing=FALSE)
                     
                     # first element represents weight of already deleted MCs!
                     cs <- cumsum(c(total_weight-sum(weights), weights[o]))
                     
                     if(weak)
                       as.integer(names(weights)[o[(cs < total_weight*noise)[-1]]])
                     else  
                       as.integer(names(weights)[o[(cs >= total_weight*noise)[-1]]])
                   },
                   
                   
                   ### FIXME: this is not exported yet
                   get_connectivity = function(matrix=FALSE) {
                     mc_weights <- weights
                     mcs <- as.integer(names(weights))
                     
                     relations <- .Call("GetRelations",rel, PACKAGE="stream")
                     
                     reltemp <- as.matrix(relations[,c(1,2)])
                     reltemp <-  matrix(match(reltemp, mcs), ncol=2) ### translate from names to index
                     val <- relations[,3]
                     
                     if(nrow(reltemp) <1) return(matrix(nrow=0, ncol=0))
                     
                     avg_weight <- apply(reltemp, MARGIN=1, FUN= function(x) mean(mc_weights[x]))
                     
                     ### similarity
                     ss <- val/avg_weight
                     ### create a distance
                     
                     ### unconnected is 2 times the largest distance
                     s <- matrix(0, ncol=length(mcs), nrow=length(mcs))
                     rownames(s) <- mcs
                     colnames(s) <- mcs
                     
                     for(i in 1:nrow(reltemp)) {
                       s[reltemp[i,1], reltemp[i,2]] <- ss[i]
                       s[reltemp[i,2], reltemp[i,1]] <- ss[i]
                     }
                     
                     strong <- .self$strong_mcs()
                     
                     s <- s[as.character(strong),as.character(strong)]
                     if(!matrix) s <- as.simil(s)
                     s
                   },
                   
                   get_membership_weights = function() {
                     s <- .self$get_connectivity()
                     
                     l <- list(description = "tNN_fast", RObj = .self)
                     class(l) <- c("DSC_tNN_fast", "DSC_Micro", "DSC_R", "DSC")
                     
                     
                     if(nrow(s)<2) assignment <- 1:nclusters(l, type="micro")
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
                       
                       d2 <- dist(get_centers(l, type="micro"), method=distFun) 
                       unconnected <- d==1
                       d[unconnected] <- d[unconnected] + d2[unconnected]
                       
                       assignment <- cutree(hclust(d, method="single"), k=k)
                     }
                     
                     ### aggregate macro-cluster weights
                     w <- get_weights(l, type="micro")
                     w <- aggregate(w, by=list(assignment), FUN=sum)$x
                     
                     ### deal with k and minweight (only if alpha is given!)
                     if(alpha>0) {
                       if(k>0 && length(w) > k) {
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
                   }
)
    
get_microclusters.DSC_tNN_fast <- function(x) {
    ### we have to rename the micro-clusters
    mc <- as.data.frame(.Call("GetAllPoints",x$RObj$flann,x$RObj$nclusters,x$RObj$columns, PACKAGE="stream"))
    if(nrow(mc)<1) return(data.frame())
    
    mc <- mc[as.character(x$RObj$strong_mcs()),]

    rownames(mc) <- NULL
    mc
}

get_microweights.DSC_tNN_fast <- function(x) {
    x$RObj$weights[as.character(x$RObj$strong_mcs())]
}



get_macroclusters.DSC_tNN_fast <- function(x) {
    if(!x$RObj$macro) stop("No macro-clusters available!")
    
    mw <-  x$RObj$get_membership_weights()
    assignment <- mw$assignment
    weights <- mw$weight
    uniqueassign <- na.omit(unique(assignment))
    
    if(length(uniqueassign) <1) return(data.frame())
    
    mcs <- get_centers(x, type="micro")
    mcw <- get_weights(x, type="micro")

    ### find weighted centroids
    as.data.frame(t(sapply(uniqueassign, FUN=function(i) {
		take <- which(assignment==i)
		colSums(mcs[take,]*mcw[take])/sum(mcw[take])	
	    })))
}

get_macroweights.DSC_tNN_fast <- function(x) {
    if(!x$RObj$macro) stop("No macro-clusters available!")
    x$RObj$get_membership_weights()$weight
}


microToMacro.DSC_tNN_fast <- function(x, micro=NULL) {
    if(is.null(micro)) micro <- 1:nclusters(x, type="micro")
    mw <- x$RObj$get_membership_weights()
   
    structure(mw$assignment[micro], names=micro)
}



### special plotting for DSC_tNN_fast
### FIXME: only show edges that really are used
plot.DSC_tNN_fast <- function(x, dsd = NULL, n = 1000,
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


    if(x$RObj$macro && type %in% c("macro")
		&& (x$RObj$columns<=2 || method=="plot")) {

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
		s <- x$RObj$get_connectivity(matrix=TRUE)
		s[lower.tri(s)] <- NA
		edges <- which(s>0, arr.ind=TRUE)
		
		if(length(edges)>0) { # length instead of nrow (s can be empty!)
		    edges <- cbind(edges, 
			    w=apply(edges, MARGIN=1, FUN=function(ij) s[ij[1], ij[2]]))

		    edges <- cbind(edges, stream:::map(edges[,3], range=c(1,5)))

		    for(i in 1:nrow(edges)){
			lines(rbind(p[edges[i,1],],p[edges[i,2],]),
				col="black",lwd=edges[i,4])
		    }
		}

	    }
	}

    }
