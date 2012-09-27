tNN_Macro_New <- setRefClass("tNN_Macro_New",
	fields = list(
		relations 		= "list",
		lambda			= "numeric",
		threshold		= "numeric",
		visited			= "numeric"),
		
		methods = list(
		initialize = function(
				threshold	= 0.05,
				lambda		= 0.01
			) {
		    
		    relations 	<<- list()
		    lambda		<<- 1 - lambda^2
		    threshold	<<- threshold
		    visited		<<- numeric()
		    
		    
		    .self
		}

	),
)

DSC_tNN_Macro_New <- function(threshold = 0.05, lambda = 0.01) {

    tNN_Macro_New <- tNN_Macro_New$new(threshold, lambda)

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
	    		relations <<- lapply(relations,function(x) {
	    			weight <- attr(x,"weight") * lambda
	    			
	    			if(weight > 0.5) {
	    				center <- attr(x,"center")
	    				x <- lapply(x, function(y) {y <- y*lambda})
	    					attr(x,"weight") <- weight
	    				attr(x,"center") <- center
	    			} else {
	    				x <- NULL
	    			}
	    			
	    			x
	    		})
	    		relations <<- Filter(Negate(is.null), relations)

	    	}
	    	
	    	if(length(relations)<1) {
	    		relations[[1]]					<<-list()
	    		attr(relations[[1]],"weight")	<<- 1
	    		attr(relations[[1]],"center")	<<- point
	    	} else {
	    		
	    		inside <- which(dist(point,as.data.frame(do.call(rbind, lapply(relations,function(x){attr(x,"center")}))))<threshold, arr.ind=TRUE)[,2]
	    		
	    		if(length(inside)>0) {
	    			partialweight <- 1/length(inside)
	    			lapply(1:length(inside), function(i) {
	    				attr(relations[[inside[i]]],"center") <<- 
	    					(attr(relations[[inside[i]]],"center")*attr(relations[[inside[i]]],"weight") + point*partialweight) /
	    					(partialweight + attr(relations[[inside[i]]],"weight"))
	    				attr(relations[[inside[i]]],"weight") <<- attr(relations[[inside[i]]],"weight") + partialweight
	    				
	    				lapply(i:length(inside), function(j) {
	    					if(i!=j) {
	    						if(is.null(unlist(relations[[inside[i]]][as.character(inside[j])]))) {
	    							relations[[inside[i]]][[as.character(inside[j])]] <<- 0
	    						}
	    						relations[[inside[i]]][[as.character(inside[j])]] <<-
	    						relations[[inside[i]]][[as.character(inside[j])]] + partialweight
	    					}
	    				})
	    			})
	    			
	    		} else {
	    			newcluster <- list()
	    			attr(newcluster,"weight") <- 1
	    			attr(newcluster,"center") <- point
	    			
	    			relations <<- c(relations, list(newcluster))
	    		}
	    	}
	    }
	}	    
)

get_microclusters.DSC_tNN_Macro_New <- function(x, ...) as.data.frame(do.call(rbind, lapply(x$RObj$relations,function(x){attr(x,"center")})))

get_centers.DSC_tNN_Macro_New <- function(x, ...) {
	assignment <- get_membership(x)
	
	mc <- get_microclusters(dsc)
	uni <- unique(assignment)
	if(length(uni) == 0)  {
		warning(paste(class(x)[1],": There are no clusters",sep=""))
		return(data.frame())
	}
	data.frame(do.call("rbind",lapply(uni,function(y) colMeans(mc[intersect(which(assignment==y),which(!is.na(mc[,1]))),]))))
	
}

get_weights.DSC_tNN_Macro_New <- function(x, scale=NULL) {
	assignment <- get_membership(x)
	weights <- do.call(rbind, lapply(dsc$RObj$relations,function(x){attr(x,"weight")}))
	
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
	edgelist <- numeric()
	r <- dsc$RObj$relations
	
	if(length(r)==0) return(numeric())
	
	(lapply(1:length(r),function(x) {
		edgelist <<- c(edgelist,x,x)
		lapply(names(sapply(r[[x]], names)),function(y) {
			if(r[[x]][[y]] > attr(r[[x]],"weight")/4) {
				edgelist <<- c(edgelist,x,y)
			}
		})
	}))
	
	if(length(edgelist)>1) {
		return(clusters(graph(edgelist,directed=FALSE))$membership)
	}
	
	1:length(r)
}

nclusters.DSC_tNN_Macro_New <- function(x) {
	unique(get_membership(x))
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

		#predict <- unlist(lapply(predict, function(y) dsc$RObj$assignment[y]))+1
		predict[is.na(predict)] <- 1	
	} else {
		warning(paste(class(x)[1],": There are no clusters",sep=""))
		predict <- rep(1,nrow(d))
	}
	predict
}