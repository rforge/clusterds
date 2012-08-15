

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

tNN_Macro <- setRefClass("tNN_Macro", 
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
		gc_ptr		= "character",
		
		horizon = "numeric"
	), 

	methods = list(
		initialize = function(
			measure	    = "Euclidean",
			distFun	    = NULL,
			minPoints   = 2,
			centroids   = TRUE,
			threshold   = 0.2,
			lambda      = 0.01,
			horizon      = 0.5
			) {
		    
		    measure	<<- measure 
		    minPoints	<<- minPoints
		    centroids   <<- centroids
		    threshold   <<- threshold
		    lambda	<<- lambda
		    lambdaFactor <<- 2^(-lambda)
		    horizon <<- horizon

		    if(!is.null(distFun)) distFun <<- distFun
		    else distFun <<- pr_DB[[measure]]

		    centers	<<- data.frame()
		    counts	<<- numeric()
		    varThresholds <<- numeric()
		    last	<<- as.character(NA)
		    
			overlap	<<- new("SimpleMC")
			global_clusters <<- list()
			gc_ptr <<- character(0)
		    
		    .self
		}

	),
)


tNN_Macro$methods(cluster = function(newdata, verbose = FALSE) {
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
		    global_clusters[["1"]] <<- "1"
		    gc_ptr[sel] <<- "1"


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
				global_clusters[[sel]] <<- sel
				gc_ptr[sel] <<- sel
			
			}else{ 
				
				
				##NEW
				## assign observation to existing node
				## max 3 clusters can be affected
				
				#cat(inside,"\n")
				
				sel_all <- names(inside)[which(inside <= 0)]
	
				## FIXME: we only update the count for the winner!
				counts[sel] <<- counts[sel] + 1
				## update counts
				#counts[sel_all] <- counts[sel_all] + 1/length(sel_all)
	
	
				if(length(sel_all)>1) {
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
					to <- gc_ptr[i[l]]
					from <- gc_ptr[j[l]]
					
					if(to != from) {
					    gc_ptr[global_clusters[[from]]] <<- to
					    global_clusters[[to]] <<- c(global_clusters[[to]], global_clusters[[from]])
					    global_clusters[[from]] <<- NULL
					}
				    }
	
	
				    ## FIXME: splitting is missing
				
				}
	
					
				
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

			    ## update counts 
			    counts[sel] <<- counts[sel] + 1
			    
			    
			    
			
			    
			}
		    }

		    last[i] <<- sel
		    
		    
		    keep <- which(counts >= horizon)
		   	remove_names <- names(counts[-keep])
		    
			lapply(remove_names,function(x){overlap <<- smc_removeState(overlap,x)})
		    
		    
		    
		    counts <<- counts[keep]
		    centers <<- centers[keep,]

		} # end for loop

		if(verbose) cat ("Done -", nclusters(x), "clusters.\n")


	    }
)

tNN_Macro$methods(clusters = function() centers[counts>minPoints,])

### creator    
DSC_tNN_Macro <- function(threshold = 0.2, minPoints = 2, measure = "euclidean",
	centroids = identical(tolower(measure), "euclidean"), lambda=0, horizon=.5) {

    tNN_Macro <- tNN_Macro$new(threshold=threshold, minPoints=minPoints, 
	    measure=measure, centroids=centroids,
	    lambda=lambda,horizon=horizon)

    l <- list(description = "tNN_Macro",
	    RObj = tNN_Macro)

    class(l) <- c("DSC_tNN_Macro","DSC_Macro","DSC_R","DSC")
    l
    l
}


### get centers
get_centers.DSC_tNN_Macro <- function(x, ...) {
	mc <- get_microclusters(x)
	uni <- unique(x$RObj$gc_ptr)
	uni <- setdiff(uni,outliers(x))
	df <- data.frame(do.call("rbind",lapply(uni,function(y) colMeans(mc[intersect(which(x$RObj$gc_ptr==y),which(!is.na(mc[,1]))),]))))
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

nclusters.DSC_tNN_Macro <- function(x)  {
	
	sum(sapply(x$RObj$global_clusters, length)>0)-length(outliers(x))
}

get_microclusters.DSC_tNN_Macro <- function(x, ...) x$RObj$clusters()

get_assignment.DSC_tNN_Macro <- function(dsc,points)  {
	d <- points
	c <- get_microclusters(dsc)
	dist <- dist(d,c)
	#Find the minimum distance and save the class
	predict <- apply(dist, 1, which.min)
	predict <- unlist(lapply(predict, function(y) dsc$RObj$gc_ptr[y]))
	predict[is.null(predict)] <- 1
	predict[is.na(predict)] <- 1
	
	predict	
}

get_weights.DSC_tNN_Macro <- function(x, scale=NULL)  {
	uni <- unique(x$RObj$gc_ptr)
	weight <- unlist(lapply(uni,function(y) mean(x$RObj$counts[which(x$RObj$gc_ptr==y)])))

    if(!is.null(scale)) weight <- map(weight, scale)
    
    weight
}


