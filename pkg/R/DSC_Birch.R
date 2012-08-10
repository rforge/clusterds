Birch <- setRefClass("Birch", 
	fields = list(
		data 		= "data.frame",
		weights 	= "numeric",
		birch	    = "ANY",
		radius	    = "numeric",
		compact	    = "numeric",
		keeptree    = "logical",
		columns	    = "ANY",
		assignment	= "numeric"
	), 

	methods = list(
		initialize = function(
			keeptree = FALSE,
			columns = NULL
			) {
		    
		    data <<- data.frame()
		    assignment <<- numeric()
		    weights <<- numeric()
		    birch <<- NULL
		    keeptree <<- keeptree 
		    columns <<- columns
		    radius <<- numeric()
		    compact <<- numeric()

		    .self
		},
		
		cluster = function(points,  weight = rep(1,nrow(points)), ...) {
			data <<- rbind(data,points)
			weights <<- c(weights,weight)
		    if(is.null(birch)) {
				birch <<- birch(data.matrix(points), 
			    	radius, compact=compact, keeptree=keeptree, 
			    	columns=columns)
			  	birch <<- birch.getTree(birch)
			} else {
		    	birch.addToTree(data.matrix(points), birch)
		    	birch <<- birch.getTree(birch)
		    }
		    
		    i<-1
		    lapply(dsc$RObj$birch$members,function(x){assignment[x]<<-i;i<<-i+1})
		    }
	)
)

### creator    
DSC_Birch <- function(radius, compact=radius, keeptree = TRUE, columns = NULL) {

    l <- list(description = "Birch",
	    RObj = new("Birch", keeptree = keeptree, columns = columns)
	    )
	    
	    l$RObj$radius <- radius
	    l$RObj$compact <- compact

    class(l) <- c("DSC_Birch","DSC_Macro","DSC_R","DSC")
    l
}

### get centers, etc.
get_centers.DSC_Birch <- function(x, ...) {
	 centers <- x$RObj$birch$sumXi/x$RObj$birch$N
	 if(length(centers)==0) warning(paste(class(x)[1],": There are no clusters",sep=""))
	 
	 as.data.frame(centers)
}

get_weights.DSC_Birch <- function(x, scale=NULL) {
    weight <- x$RObj$birch$N
    
	 if(length(weight)==0) warning(paste(class(x)[1],": There are no clusters",sep=""))

    if(!is.null(scale)) weight <- map(weight, scale)
    weight
}
