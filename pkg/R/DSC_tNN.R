#complain 


tNN <- setRefClass("tNN",
	fields = list(
		relations 		= "hash",
		lambda			= "numeric",
		r			= "numeric",
		weights			= "numeric",
		alpha			= "numeric",
		centers			= "data.frame",
		minweight		= "numeric",
		noise			= "numeric",
		killweight		= "numeric",
		k			= "numeric",
		measure			= "character",
		distFun			= "ANY",
		macro			= "logical"),


	methods = list(
		initialize = function(
			r		= 0.05,
			k		= 0,
			lambda		= 0.01,
			minweight	= .1,
			noise		= 0,
			alpha 		= 0.4,
			measure		= "Euclidean",
			macro		= TRUE
			) {

		    relations 		<<- hash()
		    lambda		<<- 2^-lambda
		    r			<<- r
		    minweight		<<- minweight
		    noise		<<- noise
		    killweight 		<<- noise*2*lambda^(10)
		    alpha		<<- alpha
		    measure		<<- measure
		    macro		<<- macro

		    if(is.null(k))
			k		<<- 0
		    else
			k		<<- k

		    weights		<<- numeric()
		    centers		<<- data.frame()

		    distFun <<- pr_DB[[measure]]

		    .self
		}

		),
	)

DSC_tNN <- function(r = 0.2, k=NULL, lambda = 0.01, minweight = .1, 
	noise = 0, alpha = .4, measure = "Euclidean", macro = TRUE) {

    tNN <- tNN$new(r, k, lambda, minweight, noise, alpha, measure, macro)

    l <- list(description = "tNN",
	    RObj = tNN)

    class <- c("DSC_tNN")

    if(macro) class <- c(class,"DSC_Macro")
    else class <- c(class,"DSC_Micro")

    class(l) <- c(class,"DSC_R","DSC")

    l
}

tNN$methods(cluster = function(newdata, verbose = FALSE) {
	    'Cluster new data.' ### online help

	    if(!is(newdata, "data.frame")) newdata <- as.data.frame(newdata)

	    wmean <- function(w) {
		if(length(w)==0)
		    return(0)
		mean(w)
	    }

	    for(i in 1:nrow(newdata)) {
		killweight <- killweight*wmean(weights)
		point <- newdata[i,]


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

		    if(macro) {
			relationWeights <- values(relations,keys)
			if(length(relationWeights) > 0) {
			    relationWeights <- relationWeights * lambda
			    if(length(which(relationWeights < killweight*alpha)))
				relations[keys[which(relationWeights < killweight*alpha)]] <- NULL
			    if(length(which(relationWeights >= killweight*alpha)))
				relations[keys[which(relationWeights >= killweight*alpha)]] <- relationWeights[which(relationWeights >= killweight*alpha)]
			}
		    }
		}

		if(nrow(centers)==0) {
		    #create first microcluster
		    weights 					 <<- 1
		    centers						 <<- rbind(centers,point)
		} else {
		    inside <- which(dist(point,centers,method=distFun)<r)


		    if(length(inside)>0) {
			partialweight <- 1 

			newCenters <- data.frame(matrix((as.numeric(as.matrix(centers[inside,])*
							rep(weights[inside])+rep(as.numeric(point)*partialweight,each=length(inside))))/
					rep(partialweight+weights[inside],ncol(point)),ncol=ncol(point)),
				row.names=rownames(centers[inside,]))

			distance <- dist(newCenters,newCenters,method=distFun)

			test <- apply(distance,1,function(x){all(x>r|x==0)})
			if(length(which(test)) > 0) {
			    centers[inside[which(test)],] <<- newCenters[which(test),]
			}

			weights[inside] <<- weights[inside] + partialweight

			if(length(inside)>1) {
			    if(macro) {
				relationUpdate <- outer(inside, inside, function(x,y){paste(x,y,sep="-")})
				relationUpdate <- relationUpdate[upper.tri(relationUpdate)]

				existingRelations <- has.key(relationUpdate,relations)
				if(length(which(existingRelations))>0)
				    relations[relationUpdate[which(existingRelations)]] <- values(relations,relationUpdate[which(existingRelations)]) + 1	
				if(length(which(!existingRelations))>0)
				    relations[relationUpdate[which(!existingRelations)]] <- 1
			    }
			}
		    } else {
			weights <<- c(weights,1)

			centers <<- rbind(centers,point)
		    }
		}
	    }
	}	    
	)

get_microclusters.DSC_tNN <- function(x) {
    mc <- x$RObj$centers
    row.names(mc) <- 1:nrow(mc)
    mc[which(x$RObj$weights>quantile(x$RObj$weights,probs=x$RObj$noise)),]
}


get_macroclusters.DSC_tNN <- function(x) {
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
			    })))

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


get_microweights.DSC_tNN <- function(x) {
    x$RObj$weights[which(x$RObj$weights>quantile(x$RObj$weights,
		    probs=x$RObj$noise))]
}


get_macroweights.DSC_tNN <- function(x) {
    m <- get_all_weights(x)

    if(x$RObj$k==0) {
	totalweight <- sum(m)
	return(m[which(m>x$RObj$minweight*totalweight)])
    } else {
	if(length(m)<x$RObj$k)
	    return(m)
	else {
	    m <- sort(m,decreasing=TRUE)
	    return(m[1:x$RObj$k])
	}
    }
}


### this needs package Matrix
#get_matrix <- function(dsc) {
#    #TODO: make edgelist max length to avoid copying
#    r <- dsc$RObj$relations
#    mc <- get_microclusters(dsc)
#    matrix <- Matrix(0,nrow(mc),nrow(mc),sparse=TRUE)
#
#    i <- 0
#
#    lookup <- sapply(rownames(mc),function(name) {
#		i <<- i + 1
#	    })
#
#    sapply(keys(dsc$RObj$relations),function(x){
#		microclusters <- unlist(strsplit(x,'-'))
#		if(all(!is.na(lookup[microclusters]))) {
#		    matrix[lookup[microclusters[1]],lookup[microclusters[2]]] <<- dsc$RObj$relations[[x]]
#		}
#	    })
#
#    matrix
#}
#
get_all_weights <- function(x) {
    assignment <- get_membership(x)
    weights <- x$RObj$weights

    nclusters <- unique(assignment)
    if(length(nclusters) == 0)  {
	warning(paste(class(x)[1],": There are no clusters",sep=""))
	return(data.frame())
    }
    m <- unlist(lapply(nclusters,function(clusters){sum(weights[which(assignment==clusters)])}))

    m
}

###get_edgelist <- function(dsc) UseMethod("get_edgelist")
get_edgelist <- function(dsc) {
    #TODO: make edgelist max length to avoid copying
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

    1:length(dsc$RObj$relations)
}

