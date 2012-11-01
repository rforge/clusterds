#TODO get shared point as a matrix. This will allow us todo other clustering on it.
#TODO go through the paper and change the name of the variables
#TODO remove tNN_Macro and replace with this

#TODO have if statement that shuts off Macro ability. Just call this
DSC_tNN. Get rid of tNN_Macro

#at the core it is micro. It's just a micro

#make the reclustering part its own macro cluster?

#change get_centers to get_macroclusters
#throw error if it is a microcluster

#move points then check if move too close. Commit changes



tNN_Macro_New <- setRefClass("tNN_Macro_New",
	fields = list(
		relations 		= "list",
		lambda			= "numeric",
		threshold		= "numeric",
		weights			= "numeric",
		alpha			= "numeric",
		centers			= "data.frame",
		minweight		= "numeric",
		noise			= "numeric",
		killweight		= "numeric",
		k				= "numeric"), #add weight and center vectors
		##TODO macro yes/no
		
		
		methods = list(
		initialize = function(
				threshold	= 0.05,
				k			= 0,
				lambda		= 0.01,
				minweight	= .1,
				noise		= 0,
				alpha 		= 0.4
			) {
		    
		    #take the measure and find measure for pr_DB[[measure]]
		    #dist(method=measure)
		    
		    relations 		<<- list()
		    lambda			<<- 2^-lambda
		    threshold		<<- threshold
		    minweight		<<- minweight
		    noise			<<- noise
		    killweight 	<<- noise*2*lambda^(10)
		    alpha			<<- alpha
		    if(is.null(k))
		    	k			<<- 0
		    else
		    	k			<<- k
		    weights		<<- numeric()
		    centers		<<- data.frame()
		    
		    
		    .self
		}

	),
)

DSC_tNN_Macro_New <- function(threshold = 0.2, k=NULL, lambda = 0.01, minweight = .1, noise = 0, alpha = .4) {

    tNN_Macro_New <- tNN_Macro_New$new(threshold, k, lambda, minweight, noise, alpha)

    l <- list(description = "tNN_Macro_New",
	    RObj = tNN_Macro_New)

    class(l) <- c("DSC_tNN_Macro_New","DSC_Macro","DSC_R","DSC")
    
    l
}

tNN_Macro_New$methods(cluster = function(newdata, verbose = FALSE) {
	    'Cluster new data.' ### online help

	    if(!is(newdata, "data.frame")) newdata <- as.data.frame(newdata)
	    
	    for(i in 1:nrow(newdata)) {
	    	killweight <- killweight*wmean(weights)
	    	point <- newdata[i,,drop = FALSE]
	    	
	    	if(lambda<1) {
	    		
	    		weights <<- weights * lambda
	    		remove <- numeric()
	    		count <- numeric()
	    		
	    		if(length(weights)>0)
	    			for(i in 1:length(weights)) {
	    				if(weights[i] >= killweight) { #micro weight needs to be the turn thing
	    					count <- 0
	    					relations[[i]] <<- lapply(relations[[i]], function(x){
	    						x <- x*lambda
	    						if(x < killweight*alpha) {
	    							count <<- count + 1
	    							return(NULL)
	    						}
	    						x
	    					})
	    					if(count > 0)
	    						relations[[i]] <<- Filter(Negate(is.null), relations[[i]])
	    				} else {
	    					remove <- c(remove,i)
	    				}
	    			}
	    		
	    		sapply(rev(remove),function(x) {
	    			relations[[x]] <<- NULL
	    		})
	    		
	    		if(length(remove)>0) {
	    			weights <<- weights[-remove]
	    			centers <<- centers[-remove]
	    			relations <<- Filter(Negate(is.null), relations)
	    		}
	    	}
	    	
	    	if(length(relations)<1) {
	    		relations[[as.character(1)]] <<- list()
	    		weights 					 <<- 1
	    		centers						 <<- rbind(centers,point)
	    	} else {
	    		#
	    		inside <- which(dist(point,centers)<threshold)
	    		if(length(inside)>0) { 
	    			
	    			partialweight <- 1 #/length(inside) #if a new data points belongs to several clusters then split evenenly
	    			lapply(1:length(inside), function(i) {
	    				name <- names(relations)[inside[i]]
	    				if(length(inside) == 1)
	    					centers[inside[i],] <<- (centers[inside[i],]*weights[inside[i]] + point*partialweight) / (partialweight + weights[inside[i]]) #update center #remove this when inside length is greater than 1 to prevent centers from overlapping
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
	    			centers <<- rbind(centers,point)
	    			
	    			relations[[as.character(as.integer(names(tail(relations, 1)))+1)]] <<-newcluster
	    		}
	    	}
	    }
	}	    
)

get_microclusters.DSC_tNN_Macro_New <- function(x, ...) {
	
	mc <- x$RObj$centers
	row.names(mc) <- 1:nrow(mc)
	mc[which(x$RObj$weights>quantile(x$RObj$weights,probs=x$RObj$noise)),]

	}

#get rid of micro clusters that have weight less

wmean <- function(w) {
	if(length(w)==0)
		return(0)
	mean(w)
}

get_centers.DSC_tNN_Macro_New <- function(x, ...) {
	assignment <- get_membership(x)
	
	mc <- get_microclusters(x)
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
	
	weights <- get_all_weights(x)
	data <- data/weights
	
	if(x$RObj$k==0) {
		totalweight <- sum(weights)
		return(data[which(weights>x$RObj$minweight*totalweight),])
	} else {
		if(nrow(data)<x$RObj$k)
			return(data)
		else {
			data <-data[order(weights,decreasing=TRUE),]
			return(data[1:x$RObj$k,])
		}
	}
}

get_weights.DSC_tNN_Macro_New <- function(x, scale=NULL) {
	m <- get_all_weights(x,scale)
	if(x$RObj$k==0) {
		totalweight <- sum(m)
		m[which(m>x$RObj$minweight*totalweight)]
	} else {
		if(length(m)<x$RObj$k)
			return(m)
		else {
			m <- sort(m,decreasing=TRUE)
			return(m[1:x$RObj$k])
		}
	}
}

get_all_weights <- function(x, scale=NULL) {
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

get_edgelist.DSC_tNN_Macro_New <- function(dsc) {
	#FIXME: make edgelist max length to avoid copying
	edgelist <- numeric()
	r <- dsc$RObj$relations
	mc <- get_microclusters(dsc)

	i <- 0
	lookupy <- sapply(names(dsc$RObj$relations),function(name) {
		i <<- i + 1
	})
	
	i <- 0
	lookupx <- sapply(row.names(mc),function(name) {
		as.numeric(name)
	})
	
	if(length(r)==0) return(numeric())
	
	(lapply(1:nrow(mc),function(x) {
		edgelist <<- c(edgelist,x,x)
		lapply(names(r[[lookupx[x]]]),function(y) {
			if(!is.null(r[[y]])) {
				yIndex <- lookupy[y]
				if(yIndex %in% lookupx) {
					if(r[[lookupx[x]]][[y]] > (dsc$RObj$weights[lookupx[x]]+dsc$RObj$weights[yIndex])/2*dsc$RObj$alpha) {
						edgelist <<- c(edgelist,x,which(lookupx==yIndex))
					}
				}
			}
		})
	}))

	edgelist
}

get_membership <- function(dsc) {
	edgelist <- get_edgelist(dsc)
	
	if(length(edgelist)>1) {
		
		assignment <- clusters(graph(edgelist,directed=FALSE))$membership
		
		return(assignment)
	}
	
	1:length(r)
}

nclusters.DSC_tNN_Macro_New <- function(x) {
	nrow(get_centers(x))
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
		warning(paste(class(dsc)[1],": There are no clusters",sep=""))
		predict <- rep(1,nrow(d))
	}
	predict
}
