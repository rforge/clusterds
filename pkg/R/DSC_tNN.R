#complain 


tNN <- setRefClass("tNN",
	fields = list(
		relations 		= "hash",
		lambda			= "numeric",
		decay_factor		= "numeric",
		r			= "numeric",
		weights			= "numeric",
		alpha			= "numeric",
		centers			= "data.frame",
		minweight		= "numeric", ### min weight for macro
		noise			= "numeric", ### min weights from micro
		killweight		= "numeric", ### for micro clusters
		k			= "numeric",
		measure			= "character",
		distFun			= "ANY",
		macro			= "logical",
		debug			= "logical"
		),


	methods = list(
		initialize = function(
			r		= 0.1,
			k		= 0,
			lambda		= 0.01,
			minweight	= 0.1,
			noise		= 0,
			alpha 		= 0.25,
			measure		= "Euclidean",
			macro		= TRUE
			) {

		    relations 		<<- hash()
		    r			<<- r
		    lambda		<<- lambda
		    decay_factor	<<- 2^-lambda
		    minweight		<<- minweight
		    noise		<<- noise
		    killweight 		<<- noise*2*decay_factor^(10)
		    alpha		<<- alpha
		    measure		<<- measure
		    macro		<<- macro

		    if(is.null(k))
			k		<<- 0
		    else
			k		<<- k

		    weights		<<- numeric()
		    centers		<<- data.frame()

		    distFun		<<- pr_DB[[measure]]

		    .self
		}

		),
	)


DSC_tNN <- function(r = 0.1, k=NULL, lambda = 0.01, minweight = 0.1, 
	noise = 0, alpha = .25, measure = "Euclidean", 
	    macro = TRUE) {

    tNN <- tNN$new(r, k, lambda, minweight, noise, alpha, measure, macro)

    l <- list(description = "tNN", RObj = tNN)

    class(l) <- c("DSC_tNN", "DSC_Micro", "DSC_R", "DSC")

    l
}

tNN$methods(cluster = function(newdata, debug = FALSE) {
	    'Cluster new data.' ### online help

	    newdata <- as.data.frame(newdata)

	    wmean <- function(w) if(length(w)==0) 0 else mean(w)

	    if(debug) cat("Debug cluster for tNN!\n")

	    for(i in 1:nrow(newdata)) {

		if(debug && !i%%100) cat("Processed",i,"points\n")

		### decay and remove clusters
		current_killweight <- killweight*wmean(weights)
		if(decay_factor<1) {
		    #decrease weight for microclusters
		    weights <<- weights * decay_factor


		    #find dead microclusters
		    remove <- which(weights < current_killweight)


		    if(length(remove)>0) {
			### get mc names
			mcs <- rownames(centers)

			if(debug) cat("  - Removing clusters",
				paste(mcs[remove], collapse=", "), 
				"\n")

			#remove microclusters
			weights <<- weights[-remove]
			centers <<- centers[-remove,]

			#remove microclusters in relations
			keys <- keys(relations)
			removekeys <- c(
				grep(paste("^",mcs[remove],"-",sep="",collapse="|"), keys, value = TRUE), 
				grep(paste("-",mcs[remove],"$",sep="",collapse="|"), keys, value = TRUE)
				)

			for(rkey in removekeys) {

			    if(debug) cat("  - Removing relation",
				    rkey, "(state)\n")

			    relations[[rkey]] <<- NULL
			}
		    }

		    if(macro) {
			### decay and remove weak relations

			if(length(relations) > 0) {
			    values(relations) <<- values(relations) * decay_factor
			    removekeys <- keys(relations)[values(relations) < current_killweight*alpha]

			    for(rkey in removekeys) {
				if(debug) cat("  - Removing relation",
					rkey, "(weight)\n")
				relations[[rkey]] <<- NULL
			    }

			}
		    }
		}

		### process new point
		point <- newdata[i,]
		mcs <- rownames(centers) ### names


		if(nrow(centers)==0) {
		    #create first micro-cluster
		    weights <<- 1
		    centers <<- as.data.frame(point)
		    rownames(centers) <<- 1
		} else {
		    inside <- which(dist(point,centers,method=distFun)<r)


		    if(length(inside)<1) { ### new cluster
			weights <<- c(weights,1)

			centers <<- rbind(centers,point)
			rownames(centers)[nrow(centers)] <<-
			as.integer(rownames(centers)[nrow(centers)-1])+1L

			if(debug) cat("  + Creating Cluster",
				rownames(centers)[nrow(centers)], "\n")

		    }else{ ### update existing cluster

			partialweight <- 1/length(inside) 

			newCenters <- data.frame(matrix((as.numeric(as.matrix(
								centers[inside,])*rep(weights[inside])+rep(as.numeric(point)*partialweight,each=length(inside))))/rep(partialweight+weights[inside],ncol(point)),
					ncol=ncol(point)),
				row.names=rownames(centers[inside,]))

			distance <- dist(newCenters,newCenters,method=distFun)

			test <- apply(distance,1,function(x){all(x>r|x==0)})
			if(length(which(test)) > 0) {
			    centers[inside[which(test)],] <<- newCenters[which(test),]
			}

			weights[inside] <<- weights[inside] + partialweight

			if(macro && length(inside)>1) {
			    relationUpdate <- outer(mcs[inside], mcs[inside], 
				    function(x,y){paste(x,y,sep="-")})
			    relationUpdate <- relationUpdate[upper.tri(relationUpdate)]

			    for(rel in relationUpdate) {
				count <- relations[[rel]]
				if(is.null(count)) count <- 1
				else count <- count +1
				    relations[[rel]] <<- count
			    }


			}
		    }
		}	   
	    }
	}
	)
    
#helper
strong_mcs <- function(x) {
    which(x$RObj$weights>quantile(x$RObj$weights,
		    probs=x$RObj$noise))
}

get_microclusters.DSC_tNN <- function(x) {
    ### we have to rename the micro-clusters
    mc <- x$RObj$centers
    if(nrow(mc)<1) return(data.frame())
    
    mc <- mc[strong_mcs(x),]

    rownames(mc) <- 1:nrow(mc)
    mc
}

get_microweights.DSC_tNN <- function(x) {
    x$RObj$weights[strong_mcs(x)]
}



get_macroclusters.DSC_tNN <- function(x) {
    if(!x$RObj$macro) stop("No macro-clusters available!")
    
    mw <-  get_membership_weights(x)
    assignment <- mw$assignment
    weights <- mw$weight

    ### we need all centers and weights here!
    mc <- x$RObj$centers
    mcw <- x$RObj$weights
    uni <- na.omit(unique(assignment))
    
    if(length(uni) == 0)  {
	return(data.frame())
    }
   
    ### find centroids
    data <- as.data.frame(t(sapply(uni, FUN=function(i) {
				take <- which(assignment==i)
				micro <- mc[take,]
				weight <- mcw[take]
				colSums(micro*weight)/sum(weight)
			    })))
    data
}

get_macroweights.DSC_tNN <- function(x) {
    if(!x$RObj$macro) stop("No macro-clusters available!")
    get_membership_weights(x)$weight
}


microToMacro.DSC_tNN <- function(x, micro=NULL) {
    if(is.null(micro)) micro <- 1:nclusters(x, type="micro")
    mw <- get_membership_weights(x)
   
    assignment <- mw$assignment[strong_mcs(x)]
    structure(assignment[micro], names=micro)
}

##################################################################3
### helpers
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

### produce an edges for all relations that surpass
### a count of alpha times the average density in the micro-clusters
get_edges <- function(dsc) {
    mc_weights <- dsc$RObj$weights
    mcs <- rownames(dsc$RObj$centers)

    if(length(mc_weights) <1) return(matrix(nrow=0, ncol=2))

    rel <- t(sapply(strsplit(keys(dsc$RObj$relations), "-"), as.integer))
    rel <-  matrix(match(rel, mcs), ncol=2) ### translate from names to index
    val <- values(dsc$RObj$relations)
   
    avg_weight <- apply(rel, MARGIN=1, FUN= function(x) mean(mc_weights[x]))
   
    rel[val >= avg_weight*dsc$RObj$alpha,, drop=FALSE]
}

### return (at most) the k largest components (k=0 -> return all)
### all need to satisfy minweight
get_membership_weights <- function(dsc) {
    k <-  dsc$RObj$k
    minweight <- dsc$RObj$minweight

    ### finds connected components
    edgelist <- get_edges(dsc)
    if(nrow(edgelist)>1) {
	edgelist <- as.integer(t(edgelist))
	assignment <- as.integer(clusters(
			graph(edgelist,directed=FALSE))$membership)
    }else {
	if(nrow(dsc$RObj$centers)<1) assignment <- integer(0)
	### each micro-cluster is its own macro cluster
	else assignment <- 1:nrow(dsc$RObj$centers)
    }

    if(length(assignment) == 0)  {
	return(list(assignment=integer(0), weight=numeric(0)))
    }

    ### gets weights of all connected components
    ### remove noise components
    weight <- dsc$RObj$weights
    weight[weight<dsc$RObj$noise] <- 0

    clusters <- unique(assignment)

    weight <- unlist(lapply(clusters, function(cl){
			sum(weight[which(assignment==cl)])
		    }))

    ### do filtring
    take <- order(weight,decreasing=TRUE)

    ### check for k
    if(k>0 && length(clusters)>k) {
	take[(k+1):length(take)] <- NA
    }

    ### check for minweight * total weight
    take[weight<(minweight*sum(weight))] <- NA

    take <- na.omit(take)
    weight <- weight[take]
    assignment <- match(assignment, take)

    list(assignment=assignment, weight=weight)
}


### special plotting for DSC_tNN
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


    #p <- get_microclusters(x)
    p <- x$RObj$centers

    if(x$RObj$macro && type %in% c("macro")
		&& (ncol(p)<=2 || method=="plot")) {
	
	if(nrow(p)>0) {

	    ### add threshold circles
	    for(i in 1:nrow(p)){
		lines(ellipsePoints(x$RObj$r, x$RObj$r, 
				loc=as.numeric(p[i,]), n=60),
			col = "black", lty=3)
	    }

	    ### add edges connecting macro-clusters
	    edges <- get_edges(x)
	    if(nrow(edges)>0) {
	    	for(i in 1:nrow(edges)){
			lines(rbind(p[edges[i,1],],p[edges[i,2],]),
				col="black")
	    	}
	    }
	    
	    points(get_centers(x, type="micro"), col="black")
	    #points(p, col=col_clusters)
	    #points(centers,col=col_clusters)
	}
    }
}
