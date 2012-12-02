kmeansW <- setRefClass("kmeansW", 
	fields = list(
		data	    = "data.frame",
		dataWeights = "numeric",
		iter.max    = "numeric",
		nstart	    = "numeric",
		assignment  = "numeric",
		k	    = "numeric",
		centers	    = "data.frame",
		weights	    = "numeric",
		details	    = "ANY"
		), 

	methods = list(
		initialize = function(
			iter.max    = 10,
			nstart	    = 1
			) {

		    data	<<- data.frame()
		    dataWeights	<<- numeric()
		    iter.max	<<- iter.max 
		    nstart	<<- nstart
		    k		<<- numeric()
		    assignment	<<- numeric()
		    weights	<<- numeric()
		    centers <<- data.frame()

		    .self
		}
		)
	)

kmeansW$methods(cluster = function(x, weight = rep(1,nrow(x)), ...) {
	    if(length(data)>0) {
		warning("KmeansW: Previous data is being overridden")
	    }
	    
	    data <<- x
	    dataWeights <<- weight
	    
	    if(nrow(x)>k) {
		kmeansW <- kmeansW(x=data, weight=dataWeights, centers=k, 
			iter.max = iter.max, nstart = nstart)

		assignment <<- kmeansW$cluster
		centers <<- data.frame(kmeansW$centers)
		details <<- kmeansW
	    } else assignment <<- 1:nrow(data)
	
	    weights <<- sapply(1:k, FUN =
		    function(i) sum(dataWeights[assignment==i], na.rm=TRUE))
	}
	)

### creator    
DSC_KmeansW <- function(k, iter.max = 10, nstart = 1) {

    kmeansW <- new("kmeansW", 
	    iter.max = iter.max, nstart = nstart)

    kmeansW$k <- k

    l <- list(description = "Weighted k-Means",
	    RObj = kmeansW)

    class(l) <- c("DSC_KmeansW","DSC_Macro","DSC_R","DSC")
    l
}

get_macroclusters.DSC_KmeansW <- function(x) x$RObj$centers
get_macroweights.DSC_KmeansW <- function(x) x$RObj$weights

