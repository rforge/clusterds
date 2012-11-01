#TODO get shared point as a matrix. This will allow us todo other clustering on it.
#TODO go through the paper and change the name of the variables
#TODO remove tNN_Macro and replace with this

#TODO have if statement that shuts off Macro ability. Just call this
#DSC_tNN. Get rid of tNN_Macro

#at the core it is micro. It's just a micro

#make the reclustering part its own macro cluster?

#change get_centers to get_macroclusters
#throw error if it is a microcluster

#move points then check if move too close. Commit changes



tNN_Macro_New <- setRefClass("tNN_Macro_New",
	fields = list(
		relations 		= "hash",
		lambda			= "numeric",
		threshold		= "numeric",
		weights			= "numeric",
		alpha			= "numeric",
		centers			= "data.frame",
		minweight		= "numeric",
		noise			= "numeric",
		killweight		= "numeric",
		k				= "numeric",
		measure			= "character",
		distFun			= "ANY"), #add weight and center vectors
		##TODO macro yes/no
		
		
		methods = list(
		initialize = function(
				threshold	= 0.05,
				k			= 0,
				lambda		= 0.01,
				minweight	= .1,
				noise		= 0,
				alpha 		= 0.4,
				measure		= "Euclidean"
			) {
				
		    relations 		<<- hash()
		    lambda			<<- 2^-lambda
		    threshold		<<- threshold
		    minweight		<<- minweight
		    noise			<<- noise
		    killweight 		<<- noise*2*lambda^(10)
		    alpha			<<- alpha
		    measure			<<- measure
		    
		    if(is.null(k))
		    	k			<<- 0
		    else
		    	k			<<- k
		    	
		    weights		<<- numeric()
		    centers		<<- data.frame()
		    
		    distFun <<- pr_DB[[measure]]
		    
		    .self
		}

	),
)

DSC_tNN_Macro_New <- function(threshold = 0.2, k=NULL, lambda = 0.01, minweight = .1, noise = 0, alpha = .4, measure = "Euclidean") {

    tNN_Macro_New <- tNN_Macro_New$new(threshold, k, lambda, minweight, noise, alpha, measure)

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
	    		#decrease weight for microclusters
	    		weights <<- weights * lambda
	    		keys <- keys(relations)
	    		
	    		#find dead microclusters
	    		remove <- which(weights < killweight)
	    		
	    		if(length(remove)>0) {
	    			#remove microclusters
	    			weights <<- weights[-remove]
	    			centers <<- centers[-remove,]
	    			removekeys <- grep(paste("^",remove,"-",sep="",collapse="|"),keys)
	    				if(length(removekeys)) {
	    				relations[keys[removekeys]]<-NULL
	    				keys <- keys[-removekeys]
	    				}
	    		}
	    		
	    		#remove dead relations
	    		#sapply(keys,function(x){
	    		#	relations[[x]] <- relations[[x]]*lambda
	    		#	if(relations[[x]] < killweight*alpha) {
	    		#		relations[[x]] <- NULL
	    		#	}
	    		#})
	    		
	    		relationWeights <- values(relations,keys)
	    		if(length(relationWeights) > 0) {
	    			relationWeights <- relationWeights * lambda
	    			if(length(which(relationWeights < killweight*alpha)))
	    				relations[keys[which(relationWeights < killweight*alpha)]] <- NULL
	    			if(length(which(relationWeights >= killweight*alpha)))
	    				relations[keys[which(relationWeights >= killweight*alpha)]] <- relationWeights[which(relationWeights >= killweight*alpha)]
	    		}
	    	}
	    	
	    	if(nrow(centers)==0) {
	    		#create first microcluster
	    		weights 					 <<- 1
	    		centers						 <<- rbind(centers,point)
	    	} else {
	    		inside <- which(dist(point,centers,method=distFun)<threshold)
	    		
	    		
	    		if(length(inside)>0) {
	    			partialweight <- 1 
	    		
	    			newCenters <- data.frame(matrix((as.numeric(as.matrix(centers[inside,])*
	    					rep(weights[inside])+rep(as.numeric(point)*partialweight,each=length(inside))))/
	    					rep(partialweight+weights[inside],ncol(point)),ncol=ncol(point)),
	    					row.names=rownames(centers[inside,]))
	    			
	    			weights[inside] <<- weights[inside] + partialweight
	    			
	    			if(length(inside)>1) {
	    				relationUpdate <- outer(inside, inside, function(x,y){paste(x,y,sep="-")})
	    				relationUpdate <- relationUpdate[upper.tri(relationUpdate)]
	    				
	    				existingRelations <- has.key(relationUpdate,relations)
	    				if(length(which(existingRelations))>0)
	    					relations[relationUpdate[which(existingRelations)]] <- values(relations,relationUpdate[which(existingRelations)]) + 1	
	    				if(length(which(!existingRelations))>0)
	    					relations[relationUpdate[which(!existingRelations)]] <- 1
	    			}
	    		} else {
	    			weights <<- c(weights,1)
	    			centers <<- rbind(centers,point)
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
	
	lookup <- sapply(rownames(mc),function(name) {
		i <<- i + 1
	 })
	
	sapply(keys(dsc$RObj$relations),function(x){
		microclusters <- unlist(strsplit(x,'-'))
		if(all(!is.na(lookup[microclusters]))) {
			if(dsc$RObj$relations[[x]] > (dsc$RObj$weights[lookup[microclusters[1]]]+dsc$RObj$weights[lookup[microclusters[2]]])/2*dsc$RObj$alpha) {
				edgelist <<- c(edgelist,lookup[microclusters[1]],lookup[microclusters[2]])
			}
		}
	})

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
