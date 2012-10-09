tNN_Macro_New <- setRefClass("tNN_Macro_New",
	fields = list(
		relations 		= "list",
		lambda			= "numeric",
		threshold		= "numeric",
		weights			= "numeric",
		alpha			= "numeric",
		centers			= "list",
		minweight		= "numeric"), #add weight and center vectors
		
		
		methods = list(
		initialize = function(
				threshold	= 0.05,
				lambda		= 0.01,
				minweight	= 0.5,
				alpha 		= 0.4
			) {
		    
		    relations 	<<- list()
		    lambda		<<- 2^-lambda
		    threshold	<<- threshold
		    minweight	<<- minweight
		    alpha		<<- alpha
		    weights		<<- numeric()
		    centers		<<- list()
		    
		    
		    .self
		}

	),
)

DSC_tNN_Macro_New <- function(threshold = 0.2, lambda = 0.2, minweight = .5) {

    tNN_Macro_New <- tNN_Macro_New$new(threshold, lambda, minweight)

    l <- list(description = "tNN_Macro_New",
	    RObj = tNN_Macro_New)

    class(l) <- c("DSC_tNN_Macro_New","DSC_Macro","DSC_R","DSC")
    
    l
}

tNN_Macro_New$methods(cluster = function(newdata, verbose = FALSE) {
	    'Cluster new data.' ### online help

	    if(!is(newdata, "data.frame")) newdata <- as.data.frame(newdata)
	    
	    for(i in 1:nrow(newdata)) {
	    	point <- newdata[i,,drop = FALSE]
	    	
	    	if(lambda>0) {
	    		
	    		weights <<- weights * lambda
	    		remove <- numeric()
	    		
	    		if(length(weights)>0)
	    			for(i in 1:length(weights)) {
	    				if(weights[i] > minweight) { 
	    					relations[[i]] <<- lapply(relations[[i]], function(x){
	    						x <- x*lambda
	    						if(x < minweight*alpha)
	    							return(NULL)
	    						x
	    					})
	    					relations[[i]] <<- Filter(Negate(is.null), relations[[i]])
	    				} else {
	    					remove <- c(remove,i)
	    				}
	    			}
	    		
	    		sapply(remove,function(x) {
	    			relations[[x]] <<- NULL
	    			centers[[x]] <<- NULL
	    			relations <<- Filter(Negate(is.null), relations)
	    		})
	    		if(length(remove)>0)
	    			weights <<- weights[-remove]
	    	}
	    	
	    	if(length(relations)<1) {
	    		relations[[as.character(1)]] <<- list()
	    		weights 					 <<- 1
	    		centers						 <<- append(centers,list(point))
	    	} else {
	    		inside <- which(dist(point,as.data.frame(do.call(rbind, centers)))<threshold)
	    		if(length(inside)>0) { 
	    			
	    			partialweight <- 1 #/length(inside) #if a new data points belongs to several clusters then split evenenly
	    			lapply(1:length(inside), function(i) {
	    				name <- names(relations)[inside[i]]
	    				if(length(inside) == 1)
	    					centers[[inside[i]]] <<- (centers[[inside[i]]]*weights[inside[i]] + point*partialweight) / (partialweight + weights[inside[i]]) #update center #remove this when inside length is greater than 1 to prevent centers from overlapping
	    				weights[inside[i]] <<- weights[inside[i]] + partialweight #weight
	    				
	    				lapply(i:length(inside), function(j) {
	    					if(i!=j) {
	    						name2 <- names(relations)[inside[j]]
	    						if(is.null(relations[[name]][[name2]])) {
	    							relations[[name]][[name2]] <<- 0
	    						}
	    						relations[[name]][[name2]] <<-
	    						relations[[name]][[name2]] + partialweight
	    						
	    						#FIXME: figure out what to add to the edges
	    						#play around with using full weight
	    					}
	    				})
	    			})
	    		} else {
	    			newcluster <- list()
	    			weights <<- c(weights,1)
	    			centers <<- append(centers, list(point))
	    			
	    			relations[[as.character(as.integer(names(tail(relations, 1)))+1)]] <<-newcluster
	    		}
	    	}
	    }
	}	    
)

get_microclusters.DSC_tNN_Macro_New <- function(x, ...) as.data.frame(do.call(rbind, x$RObj$centers))

get_centers.DSC_tNN_Macro_New <- function(x, ...) {
	assignment <- get_membership(x)
	
	mc <- get_microclusters(dsc)
	uni <- unique(assignment)
	if(length(uni) == 0)  {
		warning(paste(class(x)[1],": There are no clusters",sep=""))
		return(data.frame())
	}
	data <- data.frame(do.call("rbind",lapply(uni,function(y) {
		micro <- mc[which(assignment==y),]
		weight <- x$RObj$weights[which(assignment==y)]
		colSums(micro*weight)
	})))#colMeans(mc[intersect(which(assignment==y),which(!is.na(mc[,1]))),]))))
	
	weights <- get_weights(x)
	data <- data/weights
	
	data[which(weights>1),]
	
}

get_weights.DSC_tNN_Macro_New <- function(x, scale=NULL) {
	assignment <- get_membership(x)
	weights <- x$RObj$weights
	
	nclusters <- unique(assignment)
	if(length(nclusters) == 0)  {
		warning(paste(class(x)[1],": There are no clusters",sep=""))
		return(data.frame())
	}
	m <- unlist(lapply(nclusters,function(clusters){sum(weights[which(assignment==clusters)])}))
	
	if(!is.null(scale)) m <- map(m, scale)
	
	m
}

get_membership <- function(dsc) {
	#FIXME: make edgelist max length to avoid copying
	edgelist <- numeric()
	r <- dsc$RObj$relations
	
	i <- 0
	lookup <- sapply(names(dsc$RObj$relations),function(name) {
		i <<- i + 1
	})
	
	if(length(r)==0) return(numeric())
	
	(lapply(1:length(r),function(x) {
		edgelist <<- c(edgelist,x,x)
		lapply(names(r[[x]]),function(y) {
			if(!is.null(r[[y]])) {
				yIndex <- lookup[y]
				if(r[[x]][[y]] > (dsc$RObj$weights[x]+dsc$RObj$weights[yIndex])*alpha) {
					edgelist <<- c(edgelist,x,yIndex)
				}
			}
		})
	}))
	
	if(length(edgelist)>1) {
		
		assignment <- clusters(graph(edgelist,directed=FALSE))$membership
		
		return(assignment)
	}
	
	1:length(r)
}

nclusters.DSC_tNN_Macro_New <- function(x) {
	length(unique(get_membership(x)))
}

get_assignment.DSC_tNN_Macro_New <- function(dsc,points) {
	assignment <- get_membership(dsc)
	
	d <- points
	c <- get_microclusters(dsc)
	if(length(c)>0) {
		dist <- dist(d,c)
		#Find the minimum distance and save the class
		predict <- apply(dist, 1, which.min)
		predict <- assignment[predict]+1

		predict[is.na(predict)] <- 1	
	} else {
		warning(paste(class(x)[1],": There are no clusters",sep=""))
		predict <- rep(1,nrow(d))
	}
	predict
}