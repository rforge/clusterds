

.smc_size <- 10L
setClass("SimpleMC",
	representation(
		unused      = "integer", ## list of unused cols/rows
		top         = "integer", ## top of unused
		counts      = "matrix",
		initial_counts = "numeric" 
		),
	
	prototype(
		unused	    = .smc_size:1,
		top	    = .smc_size,
		counts	    = matrix(0, ncol=.smc_size, nrow=.smc_size),
		initial_counts = structure(rep(0, .smc_size), 
			names=rep(NA, .smc_size))  ## also holds cluster names
		)

	## FIXME: Implement check
	#validity= function(object) {
	#}
	)

tNN_Macro_New <- setRefClass("tNN_Macro_New", 
	fields = list(
		measure     = "character",
		minPoints   = "numeric",
		distFun	    = "ANY",
		centroids   = "logical",
		threshold   = "numeric",
		lambda      = "numeric",
		lambdaFactor   = "numeric",

		centers	    = "data.frame",
		counts	    = "numeric",
		varThresholds = "numeric",
		last	    = "character",
		
		overlap		= "SimpleMC",
		global_clusters = "list",
		visited = "character",
		
		beta = "numeric"
	), 

	methods = list(
		initialize = function(
			measure	    = "Euclidean",
			distFun	    = NULL,
			minPoints   = 2,
			centroids   = TRUE,
			threshold   = 0.2,
			lambda      = 0.01,
			beta      = 0.5
			) {
		    
		    measure	<<- measure 
		    minPoints	<<- minPoints
		    centroids   <<- centroids
		    threshold   <<- threshold
		    lambda	<<- lambda
		    lambdaFactor <<- 2^(-lambda)
		    beta <<- beta

		    if(!is.null(distFun)) distFun <<- distFun
		    else distFun <<- pr_DB[[measure]]

		    centers	<<- data.frame()
		    counts	<<- numeric()
		    varThresholds <<- numeric()
		    last	<<- as.character(NA)
		    
			overlap	<<- new("SimpleMC")
			global_clusters <<- list()
			visited <<- character()
		    
		    .self
		}

	),
)

nclusters.DSC_tNN_Macro_New <- function(x)  {
	length(get_centers(x))
}

tNN_Macro_New$methods(cluster = function(newdata, verbose = FALSE) {
	    'Cluster new data.' ### online help

	    if(!is(newdata, "data.frame")) newdata <- as.data.frame(newdata)
	    
	    nclusters <- function(x) nrow(centers)
	    
	    ##NEW
	    

	    last <<- character(nrow(newdata))

	    for(i in 1:nrow(newdata)) {

		nd <- newdata[i,, drop = FALSE]
		if(verbose && i%%50==0) 
		    cat("Added", i, "observations - ",
			nclusters(x), "clusters.\n")

		## fade cluster structure?
		if(lambda>0) {
		    counts <<- counts * lambdaFactor
		    smc_fade(overlap, lambdaFactor)
		    
		    
		    
		}

		## first cluster
		if(nclusters(x)<1) {
		    sel <- "1"
		    rownames(nd) <- sel
		    centers <<- nd
		    counts[sel] <<- 1 
		    ## initialize variable threshold
		    varThresholds[sel] <<- threshold
		    ##NEW
		    overlap <<- smc_addState(overlap, "1")
		    global_clusters[["1"]] <<- numeric()


		}else{
			#cat(nrow(centers)," ",length(counts),"\n")
			
		    inside <- dist(nd, centers, 
			    method=distFun) - threshold
			    
			names(inside) <- rownames(centers)
		    min <- which.min(inside)
		    
		    if(inside[min]<=0) sel <- rownames(centers)[min]
		    else sel <- NA

			## NA means no match -> create a new node
			if(is.na(sel)) {
			    ## New node
			    ## get new node name (highest node 
			    ## number is last entry in count)
			    sel <- as.character(as.integer(
					    tail(names(counts),1)) + 1)

			    rownames(nd) <- sel
			    centers <<- rbind(centers, nd)
			    counts[sel] <<- 1
			    ## initialize threshold
			    varThresholds[sel] <<- threshold
				
				#NEW
		  	  	overlap <<- smc_addState(overlap, sel)
	
				#NEW
		    	## new states cannot be part of a global cluster
				global_clusters[[sel]] <<- numeric()
			
			}else{ 
				
				
				##NEW
				## assign observation to existing node
				## max 3 clusters can be affected
				
				#cat(inside,"\n")
				
				sel_all <- names(inside)[which(inside <= 0)]
	
				## FIXME: we only update the count for the winner!
				counts[sel_all] <<- counts[sel_all] + 1/length(sel_all)
				## update counts
				#counts[sel_all] <- counts[sel_all] + 1/length(sel_all)
	
	
				#if(length(sel_all)>1) {
				    i <- rep(sel_all, length(sel_all))
				    j <- as.vector(t(matrix(sel_all, ncol=length(sel_all), length(sel_all))))
				    
				    
				    overlap <<- smc_addTransition(overlap,i,j)
				
				    ## check if the cluster should be chained
				    avg_density <- (counts[i]+
				    	    counts[j])/2
				    ## density of the denser cluster
				    #avg_density <- max(cluster_counts(cl)[i],
				    #	    cluster_counts(cl)[j])
				    ol <- smc_countMatrix(overlap)
				    ol_density <- sapply(1:length(i), 
					    FUN=function(l) ol[i[l],j[l]])
				    
				    ## hard-coded threshold
				    ## we could calculate area 
				    ## (but might not improve results!)
				    chain <- ol_density > avg_density/4
				    for(l in which(chain)) {
						if(i[l] != j[l] && !(j[l] %in% global_clusters[[i[l]]])) {
						    global_clusters[[i[l]]] <<- c(global_clusters[[i[l]]],j[l])
						    global_clusters[[j[l]]] <<- c(global_clusters[[j[l]]],i[l])
						}
				    }
				    for(l in which(!chain)) {
						if(i[l] != j[l] && j[l] %in% global_clusters[[i[l]]]) {
						    global_clusters[[i[l]]] <<- global_clusters[[i[l]]][which(global_clusters[[i[l]]]!=j[l])]
						    global_clusters[[j[l]]] <<- global_clusters[[j[l]]][which(global_clusters[[j[l]]]!=i[l])]
						}
				    }
	
	
				    ## FIXME: splitting is missing
				
				#}
	
					
				
			    ## assign observation to existing node

			    ## update center (if we use centroids)
			    if(centroids) {

				nnas <- !is.na(nd)
				centers[sel,nnas] <<- 
				(centers[sel,nnas] * counts[sel] 
					+ nd[nnas])/(counts[sel]+1)
				nas <- is.na(centers[sel,])
				centers[sel,nas] <<- nd[nas]

			    }

			    
			    
			
			    
			}
		    }

		    last[i] <<- sel
		    
		 
		    

		} # end for loop

		if(verbose) cat ("Done -", nclusters(x), "clusters.\n")


	    }
)

tNN_Macro_New$methods(clusters = function() centers[counts>minPoints,])

### creator    
DSC_tNN_Macro_New <- function(threshold = 0.2, minPoints = 2, measure = "euclidean",
	centroids = identical(tolower(measure), "euclidean"), lambda=0, beta=.5) {

    tNN_Macro_New <- tNN_Macro_New$new(threshold=threshold, minPoints=minPoints, 
	    measure=measure, centroids=centroids,
	    lambda=lambda,beta=beta)

    l <- list(description = "tNN_Macro_New",
	    RObj = tNN_Macro_New)

    class(l) <- c("DSC_tNN_Macro_New","DSC_Macro","DSC_R","DSC")
    l
    l
}

get_macroclusters <- function(x, ...) {
	x$RObj$visited <- unlist(as.character(outliers(x)))
	gc <- sapply(1:length(x$RObj$global_clusters),function(current){list(unlist(depth_search(x,current)))})
	gc[unlist(lapply(gc,length)!=0)]
}

depth_search <- function(x, current) {
	if(current %in% x$RObj$visited)
		return(NULL)
	
	x$RObj$visited <- c(x$RObj$visited,as.character(current))
	
	selected <- sapply(x$RObj$global_clusters[[current]],function(c){depth_search(x,c)})
	
	selected <- c(selected, as.character(current))
	unlist(selected)
}

### get centers
get_centers.DSC_tNN_Macro_New <- function(x, ...) {
	mc <- get_microclusters(x)
	macro <-get_macroclusters(x)
	df <- data.frame(t(sapply(macro,function(x){temp <- sapply(x,function(y){as.numeric(mc[y,])});rowMeans(as.matrix(temp[,!is.na(temp[1,])]))})))
	df[!is.na(df[,1]),]
}

outliers <- function(x) {
	    # single unconnected micro-cluster
	    ol <- unlist(x$RObj$global_clusters[which(sapply(x$RObj$global_clusters, length)==1)])

	    ## in the lowesed .25 quantile 
	    ol[x$RObj$counts[ol]<quantile(x$RObj$counts, .25)]

	    ## low density
	    #clusters(x)[cluster_counts(x)<quantile(cluster_counts(x), .25)]
	
}

get_microclusters.DSC_tNN_Macro_New <- function(x, ...) x$RObj$clusters()

get_assignment.DSC_tNN_Macro_New <- function(dsc,points)  {
	d <- points
	c <- get_microclusters(dsc)
	dist <- dist(d,c)
	#Find the minimum distance and save the class
	predict <- apply(dist, 1, which.min)
	gc_ptr <- unlist(sapply(1:length(macro),function(x){names(macro[[x]])<-rep("",length(macro[[x]]));sapply(macro[[x]],function(y){sapply(y,function(z){x})})}))
	predict <- unlist(lapply(predict, function(y) gc_ptr[as.character(y)]))
	predict[is.null(predict)] <- 1
	predict[is.na(predict)] <- 1
	
	predict	
}

get_weights.DSC_tNN_Macro_New <- function(x, scale=NULL)  {
	uni <- unique(x$RObj$gc_ptr)
	weight <- unlist(lapply(uni,function(y) mean(x$RObj$counts[which(x$RObj$gc_ptr==y)])))

    if(!is.null(scale)) weight <- map(weight, scale)
    
    weight
}


